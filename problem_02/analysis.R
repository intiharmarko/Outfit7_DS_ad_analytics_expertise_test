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



# EDA (Initial checks)
# - do column names match with expected column names
# - check column types (any strange column types)
# - check missing rows
# - (check any strange rows (that do not follow the structure of other rows))


## column names - match test

### expected columns df
df.cols <- expect_cols_table()

### execute matching test (per file)
col_name_matching(df.sn_170915, "SuperNetwork (SN) - 2017-09-15")
col_name_matching(df.sn_170916, "SuperNetwork (SN) - 2017-09-16")
col_name_matching(df.au_170915, "AdUmbrella (AU) - 2017-09-15")
col_name_matching(df.au_170916, "AdUmbrella (AU) - 2017-09-16")


## column types - check
cols_types(df.sn_170915, "SuperNetwork (SN) - 2017-09-15")
cols_types(df.sn_170916, "SuperNetwork (SN) - 2017-09-16")
cols_types(df.au_170915, "AdUmbrella (AU) - 2017-09-15")
cols_types(df.au_170916, "AdUmbrella (AU) - 2017-09-16")


## missing rows - check
rows_missing(df.sn_170915, "SuperNetwork (SN) - 2017-09-15")
rows_missing(df.sn_170916, "SuperNetwork (SN) - 2017-09-16")
rows_missing(df.au_170915, "AdUmbrella (AU) - 2017-09-15")
rows_missing(df.au_170916, "AdUmbrella (AU) - 2017-09-16")

### show missing rows
df.au_170915 %>% filter(if_any(everything(), is.na))
df.au_170916 %>% filter(if_any(everything(), is.na))



# Data pre-processing (before main EDA)
# - before we enter main EDA
# - we need to apply additional cleaning steps:
#   - use identical column names over all dfs
#   - clean revenue column (move currency unit into separate column)
#   - remove rows with totals 
#   - convert data column to date type

## apply cleaning (we create new df instance!)
df.sn_170915.c <- clean_df(df.sn_170915)
df.sn_170916.c <- clean_df(df.sn_170916)
df.au_170915.c <- clean_df(df.au_170915)
df.au_170916.c <- clean_df(df.au_170916)


