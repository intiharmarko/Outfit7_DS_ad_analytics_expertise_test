#' Helper Functions for EDA Main

require(tidyverse)
#require(patchwork)



#' Visualize bar plot of selected categorical column.
#' 
#' 
#' @param df.merged_ A data frame - all sources merged.
#' @param col A column / variable for which we draw bar plot.
#' @param col_name A string of column / variable name shown on the plot.
#' @param cols_sources_ A vector of color names (data sources).
#' @param font_base_size_ An integer - base font size on the plot. 
#' @param log_scale_ A logical - TRUE (apply log10 scaling) | FALSE (do not apply log10 scaling). 
#'
#' @return None
#'
plot_cat_var_bar <- function(df.merged_,
                             col,
                             col_name,
                             cols_sources_ = cols_sources,
                             font_base_size_ = font_base_size,
                             log_scale_ = T){
  
  df.merged_ %>% 
    group_by(source, {{ col }}) %>% 
    summarise(n = n(), 
              .groups = "drop") %>% 
    ggplot(aes(x = {{ col }},
               y = n,
               fill = source)) +
    geom_col(color = "black", 
             show.legend = F) + 
    facet_grid(cols = vars(source)) +
    scale_fill_manual(values = cols_sources_) +
    xlab(ensym(col_name)) +
    ylab("Frequency (row counts)") +
    ggtitle(paste("Distribution of", ensym(col_name))) +
    theme_minimal(base_size = font_base_size_) +
    theme(axis.text.x = element_text(angle = 90))
}


#' Visualize density plot of selected numeric (float) column.
#' 
#' @param df.merged_ A data frame - all sources merged.
#' @param col A column / variable for which we draw density plot.
#' @param col_name A string of column / variable name shown on the plot.
#' @param cols_sources_ A vector of color names (data sources).
#' @param font_base_size_ An integer - base font size on the plot. 
#' @param log_scale_ A logical - TRUE (apply log10 scaling) | FALSE (do not apply log10 scaling). 
#'
#' @return None
#'
plot_num_var_density <- function(df.merged_,
                                 col,
                                 col_name,
                                 cols_sources_ = cols_sources,
                                 font_base_size_ = font_base_size,
                                 log_scale_ = T){

  
  # create plot (with or without log10 scaling)
  if(log_scale_ == T){
    
    df.merged_ %>% 
      ggplot(aes(x = {{ col }},
                 fill = source)) +
      geom_density(color = "black", 
                   show.legend = F) + 
      scale_x_log10() +
      facet_grid(rows = vars(source)) +
      scale_fill_manual(values = cols_sources_) +
      xlab(paste0(ensym(col_name), " - log10 scale")) +
      ylab("Density") +
      ggtitle(paste("Distribution of", ensym(col_name))) +
      theme_minimal(base_size = font_base_size_)
    
  } else if(log_scale_ == F){
    df.merged_ %>% 
      ggplot(aes(x = {{ col }},
                 fill = source)) +
      geom_density(color = "black", 
                   show.legend = F) + 
      facet_grid(rows = vars(source)) +
      scale_fill_manual(values = cols_sources_) +
      xlab(paste0(ensym(col_name))) +
      ylab("Density") +
      ggtitle(paste("Distribution of", ensym(col_name))) +
      theme_minimal(base_size = font_base_size_)
    
  } else{
    message("Please provide boolean value for parameter parameter 'log_scale_'!")
  }
}



#' Visualize logical consistency checks results
#' - number of violations per each source
#' 
#' @param df.merged_ A data frame - all sources merged.
#' @param cols_sources_ A vector of color names (data sources).
#' @param font_base_size_ An integer - base font size on the plot. 
#'
#' @return None
#'
plot_logical_consistency <- function(df.merged_,
                                     cols_sources_ = cols_sources,
                                     font_base_size_ = font_base_size){
  
  df.merged_ %>%  
    group_by(source) %>% 
    summarise(`nr: impressions > requests` = sum(`f: impressions > requests`),
              `nr: requests < 0`           = sum(`f: requests < 0`),
              `nr: impressions < 0`        = sum(`f: impressions < 0`),
              `nr: revenue < 0`            = sum(`f: revenue < 0`), 
              .groups = "drop") %>% 
    pivot_longer(!source, 
                 names_to = "violation", 
                 values_to = "count") %>% 
    ggplot(aes(x = violation, 
               y = count, 
               fill = source)) +
    geom_col(color = "black", 
             show.legend = F) +
    facet_grid(rows = vars(source)) +
    scale_fill_manual(values = cols_sources_) +
    xlab("Logical violation") +
    ylab("Number of violations") +
    ggtitle("Logical inconsistency violations check") +
    labs(subtitle = "Counts above 0 show violations (values that shouldn't exist)!") +
    theme_minimal(base_size = font_base_size_)
}



#' visualize distribution for fill rate
#' - break down by source
#' 
#' @param df.merged_ A data frame - all sources merged.
#' @param cols_sources_ A vector of color names (data sources).
#' @param font_base_size_ An integer - base font size on the plot. 
#'
#' @return None
#'
plot_distr_fill_rate <- function(df.merged_,
                                 cols_sources_ = cols_sources,
                                 font_base_size_ = font_base_size){
  
  df.merged_ %>% 
    ggplot(aes(x = fill_rate,
               fill = source)) +
    geom_density(color = "black") +
    facet_wrap(vars(source)) +
    scale_fill_manual(values = cols_sources_) +
    xlab("Fill rate (impressions / requests)") +
    ylab("Density") +
    ggtitle("Fill rate distribution break down by source") +
    labs(subtitle = "Fill rate should have values on the range [0,1]!",
         fill = "Source:") +
    theme_minimal(base_size = font_base_size)
}



#' visualize distribution for eCPM rate 
#' - break down by source
#' 
#' @param df.merged_ A data frame - all sources merged.
#' @param cols_sources_ A vector of color names (data sources).
#' @param font_base_size_ An integer - base font size on the plot. 
#'
#' @return None
#'
plot_distr_eCPM <- function(df.merged_,
                            cols_sources_ = cols_sources,
                            font_base_size_ = font_base_size){
  
  df.merged_ %>% 
    ggplot(aes(x = eCPM,
               fill = source)) +
    geom_density(color = "black") +
    geom_text(aes(label = currency, 
                  x = 0.5, 
                  y = 0.5), 
              size = 16) +
    facet_wrap(vars(source)) +
    scale_fill_manual(values = cols_sources_) +
    xlab("eCPM") +
    ylab("Density") +
    ggtitle("Effective Cost per Mille distribution break down by source") +
    labs(subtitle = "eCPM = revenue / impressions * 100\nCurrency shown on the graph!",
         fill = "Source:") +
    theme_minimal(base_size = font_base_size)
  
}



#' visualize distribution for eCPM rate 
#' - break down by source
#' 
#' @param df.merged_ A data frame - all sources merged.
#' @param cols_sources_ A vector of color names (data sources).
#' @param font_base_size_ An integer - base font size on the plot. 
#'
#' @return None
#'
plot_distr_eCPM <- function(df.merged_,
                            cols_sources_ = cols_sources,
                            font_base_size_ = font_base_size){
  
  df.merged_ %>% 
    ggplot(aes(x = eCPM,
               fill = source)) +
    geom_density(color = "black") +
    geom_text(aes(label = currency, 
                  x = 0.5, 
                  y = 0.5), 
              size = 16) +
    facet_wrap(vars(source)) +
    scale_fill_manual(values = cols_sources_) +
    xlab("eCPM") +
    ylab("Density") +
    ggtitle("Effective Cost per Mille distribution break down by source") +
    labs(subtitle = "eCPM = revenue / impressions * 100\nCurrency shown on the graph!",
         fill = "Source:") +
    theme_minimal(base_size = font_base_size)
}



#' Visualize instances counts
#' - per selected primary key
#' - break down by source
#' 
#' @param df.merged_ A data frame - all sources merged.
#' @param cols_sources_ A vector of color names (data sources).
#' @param font_base_size_ An integer - base font size on the plot. 
#'
#' @return None
#'
plot_instances_count <- function(df.merged_,
                                 cols_sources_ = cols_sources,
                                 font_base_size_ = font_base_size){
  
  df.merged_ %>% 
    distinct(source, date, app, platform, nr_rows) %>% 
    ggplot(aes(x = nr_rows,
               fill = source)) +
    geom_histogram(binwidth = 1,
                   color = "black") +
    facet_wrap(vars(source)) +
    scale_fill_manual(values = cols_sources_) +
    xlab("Number of instances") +
    ylab("Frequency") +
    ggtitle("Number of multiple instances per selected primary key") +
    labs(subtitle = "Selected primary key: source ~ date ~ app ~ platform\nMore than 1 instance probably indicates we don't see full primary key,\nor we have duplicated values.",
         fill = "Source:") +
    theme_minimal(base_size = font_base_size)
}






