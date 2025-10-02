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
