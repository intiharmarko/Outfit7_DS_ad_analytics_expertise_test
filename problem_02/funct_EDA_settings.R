#' EDA settings
#' - figure settings (colors, theme, etc.)
#' - figure export settings (size, naming conventions, etc)


#' Plot objects
line_size   <- 0.8
point_size  <- 3
line_color  <- "gray50"
point_color <- "gray50"
fill_color  <- "gray50"

#' Categories - colors
cols_sources <- c("burlywood2", "burlywood4", "coral1", "coral4") 

#' Fonts
font_base_size <- 16

#' Figures
fig_width  <- 30
fig_height <- 21
fig_dpi    <- 600


#' Export figure
#' - this functions exports figure on local drive
#' 
#' @param fig_name A string - figure name.  
#' @param fig_dir A string - path to figure directory  
#' @param width A number - figure width in cm (unit).  
#' @param height A number - figure height in cm (unit). 
#' @param dpi A number - figure resolution in dpi (unit). 
#'
#' @return None 
#' 
export_fig  <- function(fig_name,
                        fig_dir,
                        width = fig_width,
                        height = fig_height, 
                        dpi = fig_dpi){
  
  ggsave(filename = paste0(fig_dir, fig_name), 
         plot = last_plot(), 
         device = "png", 
         units = "cm", 
         width = fig_width, 
         height = fig_height, 
         dpi = fig_dpi)
}
