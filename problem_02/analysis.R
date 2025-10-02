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



# EDA (Initial structure checks)
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
df.c.merged %>% 
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
  xlab("Logical violation") +
  ylab("Number of violations") +
  ggtitle("Logical inconsistency violations check") +
  labs(subtitle = "Counts above 0 show violations (values that shouldn't exist)!") +
  theme_minimal(base_size = 16)


## Metrics check
## - first we calculate two additional metrics:
##   - effective cost per mille (eCPM)
##   - fill rate
## - then we visualize both distributions and check for potential anomalies

### calculate metrics
df.c.merged <- calc_metrics(df.c.merged)

### visualize distribution for fill rate break down by source
df.c.merged %>% 
  ggplot(aes(x = fill_rate,
             fill = source)) +
  geom_density(color = "black") +
  facet_wrap(vars(source)) +
  xlab("Fill rate (impressions / requests)") +
  ylab("Density") +
  ggtitle("Fill rate distribution break down by source") +
  labs(subtitle = "Fill rate should have values on the range [0,1]!",
       fill = "Source:") +
  theme_minimal(base_size = 16)

### visualize distribution for eCPM rate break down by source
df.c.merged %>% 
  ggplot(aes(x = eCPM,
             fill = source)) +
  geom_density(color = "black") +
  geom_text(aes(label = currency, 
                x = 0.5, 
                y = 0.5), 
            size = 16) +
  facet_wrap(vars(source)) +
  xlab("Fill rate (impressions / requests)") +
  ylab("Density") +
  ggtitle("Effective Cost per Mille distribution break down by source") +
  labs(subtitle = "eCPM = revenue / impressions * 100\nCurrency shown on the graph!",
       fill = "Source:") +
  theme_minimal(base_size = 16)


## Check duplicates & aggregates
## - first we will check if duplicates exists per each source
## - duplicate row definition: if multiple rows exists for single value source ~ date ~ app ~ platform
## - we will count how many duplicates (and shown which)
## - then we will aggregate merge df
## - and recalculate violation flags and metrics
## - and we will repeat previous two EDA steps for aggregates

### count rows per selected primary key (source ~ date ~ app ~ platform)
df.c.merged <- df.c.merged %>% 
  select(source:currency) %>% 
  arrange(source, date, app, platform) %>% 
  group_by(source, date, app, platform) %>% 
  mutate(row_id = row_number(),
         nr_rows = n()) %>% 
  ungroup()

### visualize counts
df.c.merged %>% 
  distinct(source, date, app, platform, nr_rows) %>% 
  ggplot(aes(x = nr_rows,
             fill = source)) +
  geom_histogram(binwidth = 1,
                 color = "black") +
  facet_wrap(vars(source)) +
  xlab("Number of instances") +
  ylab("Frequency") +
  ggtitle("Number of multiple instances per selected primary key") +
  labs(subtitle = "Selected primary key: source ~ date ~ app ~ platform\nMore than 1 instance probably indicates we don't see full primary key,\nor we have duplicate values.",
       fill = "Source:") +
  theme_minimal(base_size = 16)

### check duplicates over all relevant columns
df.c.merged %>% 
  group_by(source, date, app, platform, requests, impressions, revenue, currency) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  filter(n > 1) %>% 
  nrow()
