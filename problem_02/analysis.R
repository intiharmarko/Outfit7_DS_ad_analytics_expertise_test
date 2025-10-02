# Problem 2: Analyzing Suspicious Data - main analysis script
#
# - goal: Find a report (data in .csv file) that is not ok!
# - we will use different EDA techniques for detecting faults in the given data
# - final summary of the analysis is provided in the .pdf report (in directory /ppt)

rm(list = ls())
graphics.off()


# Libraries
library(tidyverse)

# Load custom functions
source("./problem_02/functions.R")

# Paths
path_data <- "./problem_02/data/"



# Data
# - two different ad networks reported files
#   - SuperNetwork (SN)
#   - AdUmbrella (AU)
# - files are reported for two different dates:
#   - 2017-09-15
#   - 207-09-16

## file names
file_sn_170915 <- "2017-09-15.csv"
file_sn_170916 <- "2017-09-16.csv"
file_au_170915 <- "adumbrella-15_9_2017.csv"
file_au_170916 <- "adumbrella-16_9_2017.csv"
  
## import data
df.sn_170915 <- read_csv(file = paste0(path_data, file_sn_170915), col_names = T)
df.sn_170916 <- read_csv(file = paste0(path_data, file_sn_170916), col_names = T)
df.au_170915 <- read_csv(file = paste0(path_data, file_au_170915), col_names = T)
df.au_170916 <- read_csv(file = paste0(path_data, file_au_170916), col_names = T)



# Exploratory data analysis (EDA)
# - EDA is split into multiple parts
#   - initial data checks
#   - 

# EDA - Initial checks
# - do column names match with expected column names
# - check column types (any strange column types)
# - check missing rows
# - check any strange rows (that do not follow the structure of other rows)


## column names - match test

### expected columns df
df.cols <- expect_cols_table()

### execute matching test (per file)
col_name_matching(df.sn_170915, "SuperNetwork (SN) - 2017-09-15")
col_name_matching(df.sn_170916, "SuperNetwork (SN) - 2017-09-16")
col_name_matching(file_au_170915, "AdUmbrella (AU) - 2017-09-15")
col_name_matching(file_au_170916, "AdUmbrella (AU) - 2017-09-16")
