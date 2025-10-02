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
source("./problem_02/funct_preproc.R")
source("./problem_02/funct_EDA.R")
source("./problem_02/funct_EDA_settings.R")


# Paths
path_data <- "./problem_02/data/"
path_fig  <- "./problem_02/fig/"



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



# EDA (Initial structure checks)
# - do column names match with expected column names
# - check column types (any strange column types)
# - check missing rows
# - (check any strange rows (that do not follow the structure of other rows))
# - check for duplicates 8identical rows


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

### check duplicates (identical rows)
duplicates_count(df.sn_170915, "SuperNetwork (SN) - 2017-09-15")
duplicates_count(df.sn_170916, "SuperNetwork (SN) - 2017-09-16")
duplicates_count(df.au_170915, "AdUmbrella (AU) - 2017-09-15")
duplicates_count(df.au_170916, "AdUmbrella (AU) - 2017-09-16")


# Data pre-processing (before main EDA)
# - before we enter main EDA
# - we need to apply additional cleaning steps:
#   - use identical column names over all dfs
#   - clean revenue column (move currency unit into separate column)
#   - remove rows with totals 
#   - convert data column to date type
# - also we merge all cleaned dfs into a single df 

## apply cleaning (we create new df instance!)
df.sn_170915.c <- clean_df(df.sn_170915)
df.sn_170916.c <- clean_df(df.sn_170916)
df.au_170915.c <- clean_df(df.au_170915)
df.au_170916.c <- clean_df(df.au_170916)

## merge dfs
df.c.merged <- list(SuperNetwork_170915 = df.sn_170915.c,
                    SuperNetwork_170916 = df.sn_170916.c,
                    AdUmbrella_170915 = df.au_170915.c,
                    AdUmbrella_170916 = df.au_170916.c) %>% 
  bind_rows(.id = "source") %>% 
  mutate(source = factor(source, levels = c("SuperNetwork_170915", 
                                            "SuperNetwork_170916", 
                                            "AdUmbrella_170915", 
                                            "AdUmbrella_170916")))



# EDA main
# - Split into multiple steps:
#   - logical consistency check
#   - metrics
#   - check duplicates & aggregates
#   - cross-day analysis
#   - cross-network analysis


## Logical consistency check
## - inspect logical violations for requests, impressions and revenues
## - and flag violations
## - first we calculate flags for each df
## - then we create summary of violation results

### add flags
df.c.merged <- logical_consis_checks(df.c.merged)

### visualize logical consistency checks results
### - we show number of violations per each df
plot_logical_consistency(df.c.merged); export_fig("02_violation_flags.png", path_fig)



## Metrics check
## - first we calculate two additional metrics:
##   - effective Cost Per Mille (eCPM)
##   - fill rate
## - then we visualize both distributions and check for potential anomalies

### calculate metrics
df.c.merged <- calc_metrics(df.c.merged)

### visualize distribution for fill rate break down by source
plot_distr_fill_rate(df.c.merged); export_fig("03_fill_rate_distr.png", path_fig)

### visualize distribution for eCPM rate break down by source
plot_distr_eCPM(df.c.merged); export_fig("03_fill_eCPM.png", path_fig)


## Check duplicates & aggregates
## - first we will check if duplicates exists per each source
## - duplicate row definition: if multiple rows exists for single value source ~ date ~ app ~ platform
## - we will count how many duplicates (and shown which)
## - then we will aggregate merge df
## - and recalculate violation flags and metrics
## - and we will repeat previous two EDA steps for aggregates

### count rows per selected primary key (source ~ date ~ app ~ platform)
df.c.merged <- add_instances_counts(df.c.merged)

### visualize counts
plot_instances_count(df.c.merged); export_fig("04_intances_counts.png", path_fig)

### aggregate data on primary key level
### - and re-calculate violations flags and metrics
### - repeat visualizations
df.c.merged.aggr <- aggregate_df(df.c.merged)
  
### visualize logical consistency checks results (aggregated data)
plot_logical_consistency(df.c.merged.aggr); export_fig("04_aggr_violation_flags.png", path_fig)

### visualize distribution for fill rate break down by source (aggregated data)
plot_distr_fill_rate(df.c.merged.aggr); export_fig("04_aggr_fill_rate_distr.png", path_fig)

### visualize distribution for eCPM rate break down by source (aggregated data)
plot_distr_eCPM(df.c.merged.aggr); export_fig("04_aggr_fill_eCPM.png", path_fig)
