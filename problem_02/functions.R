#Problem 2: Analyzing Suspicious Data - helper functions


#' Load packages
library(tidyverse)


#' Expected columns
#'
#' Create a df of expected columns based on problem description (which columns we should have in the data).
#' - The df includes:
#'   - expected data (based on problem description)
#'   - string for column naming matching (the most obvious one)
#' 
#' @return df.cols A data frame - with expected column names.
#'
expect_cols_table <- function(){
  
  df.cols <- tribble(~`expected data`,          ~`match string`,
                     "reporting date",                   "date",
                     "app id",                            "app",
                     "platform (iOS, Android)",      "platform",
                     "amount of revenue",             "revenue",
                     "number of ad requests",         "request",
                     "impressions",                   "impress")
  
  return(df.cols)
}



#' Column names matching test
#'
#' Check if column names match the expected column names from generated table.
#' 
#' @param df_ A data frame - imported df (for which we apply match test).
#' @param data_name_ A string - name of the imported dataset.
#' @param df.cols_ A data frame - with expected column names.
#' 
#' @return NULL
#'
col_name_matching <- function(df_,
                              data_name_,
                              df.cols_ = df.cols){
  
  # regular expression for column names match
  reg_match <- df.cols_ %>% pull(`match string`) %>% paste(collapse = "|")
  
  cat("\n-----------------------")
  cat(paste0("\nColumn names matching test - data frame (", data_name_,")"))
  
  df.match <- tibble(col_name = df_ %>% colnames() %>% str_to_lower()) %>% 
    mutate(matched = str_detect(string = col_name, pattern = reg_match))
  
  nr_not_matched <- df.match %>% 
    filter(!matched) %>% 
    nrow()
  
  if(nr_not_matched == 0){
    cat("\nAll expected columns are present in the df!")
  } else{
    cat("\nSome columns are not present in the df\n(not included or different column name than expected!\nCheck the column list below that don't match the name:)\n")
    print(df.match %>% filter(!matched))
  }
}


#' Report column types
#'
#' Extract column names and types from given df and report it.
#' 
#' @param df_ A data frame - imported df.
#' @param data_name_ A string - name of the imported dataset.
#' 
#' @return NULL
#'
cols_types <- function(df_,
                       data_name_){
  
  df_types <- imap_dfr(df_, ~tibble(
    col_name = .y,
    col_type = class(.x)[1]
  ))
  
  cat("\n-----------------------")
  cat(paste0("\nColumn types - data frame (", data_name_,")"))
  print(df_types)
}


#' Report missign rows
#'
#' Count number of missing rows per each column in df and report it.
#' 
#' @param df_ A data frame - imported df.
#' @param data_name_ A string - name of the imported dataset.
#' 
#' @return NULL
#'
rows_missing <- function(df_,
                         data_name_){
  
  df_missing <- imap_dfr(df_, ~tibble(
    col_name = .y,
    n_missing = sum(is.na(.x))
  ))
  
  cat("\n-----------------------")
  cat(paste0("\nNumber of missign rows per column - data frame (", data_name_,")"))
  print(df_missing)
}



#' Create clean data frame
#'
#' We need some pre-processing before entering main EDA stage:
#   - use identical column names over all dfs
#   - clean revenue column (move currency unit into separate column)
#   - remove rows with totals 
#   - convert date column to date type
#' 
#' @param df_ A data frame - imported df.
#' 
#' @return df_ A data frame - cleaner version.
#'
clean_df <- function(df_){
  
  # clean column names
  colnames(df_) <- colnames(df_) %>% str_to_lower() %>% sub(" .*", "", .)
  
  # fix revenue column (currency unit moved to separate column)
  # - move currency symbol to new column if symbol present
  # - if currency symbol not present we assume currency is USD
  # - convert revenue column to numeric
  df_ <- df_ %>%
    mutate(
      currency = str_extract(revenue, "€|\\$|£"),           
      currency = if_else(is.na(currency), "$", currency),   
      revenue = str_remove(revenue, "€|\\$|£"),            
      revenue = as.numeric(revenue)                        
    ) 
  
  # remove final row with totals
  df_ <- df_ %>% 
    filter(date != "Totals")
  
  # convert date to date type
  df_ <- df_ %>% 
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  return(df_)
}



#' Calculate logical consistency checks
#'
#' We add some flags that check if given value is as expected.
#' - checks applied:
#'   - impressions > requests (not logical)
#'   - requests < 0           (not logical)
#'   - impressions < 0        (not logical)
#'   - revenue < 0            (not logical)
#' 
#' @param df_ A data frame - imported df.
#' 
#' @return df_ A data frame - with checks added.
#'
logical_consis_checks <- function(df_){
  
  df_ <- df_ %>% 
    mutate(`f: impressions > requests` = if_else(impressions > requests, 1, 0),
           `f: requests < 0`           = if_else(requests < 0, 1, 0),
           `f: impressions < 0`        = if_else(impressions < 0, 1, 0),
           `f: revenue < 0`            = if_else(revenue < 0, 1, 0))
  
  return(df_)
}

  