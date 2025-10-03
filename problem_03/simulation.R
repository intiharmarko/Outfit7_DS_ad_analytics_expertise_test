# Problem 3: Ad-Mediation Optimization
#
# - goal: Find optimal sorting order for ads providers companies (based on expected user revenue).
# - we will demonstrate three different approaches
#   - Approach 1: empirical: MC simulation (all combinations)
#   - Approach 2: theoretical estimation (all combinations)
#   - Approach 3: smart approach (efficient sorting of companies based on revenues per impression)

rm(list = ls())
graphics.off()


# Libraries
library(tidyverse)
library(gtools)
library(patchwork)

# Load custom functions
source("./problem_03/funct_sim.R")
source("./problem_02/funct_EDA_settings.R")


# Paths
path_fig  <- "./problem_03/fig/"


# Global variables (inputs)
comp_prob_fill <-  c(0.10, 0.19, 0.40, 0.45, 0.50) # fill rate (probability) - ad companies
comp_rev_exp   <- c(10,  5,  4,  3,  5)            # actual expected revenue - ad companies
nr_comp        <- length(comp_prob_fill)           # number of ad provider companies 
n_users        <- 10000                             # number of users (simulation parameter)
n_reps         <- 1000                            # number of repetitions (simulation parameter)

## table: companies
df.comp <- tibble(id = paste0("C", 1:nr_comp),
                  p  = comp_prob_fill,  
                  r  = comp_rev_exp)


# Simulation step:
# - we estimate expected user revenue based on two approaches
#   - approach 1: empirical expected user revenue based on Monte Carlo simulation
#   - approach 2: theoretical expected user revenue based theoretical distribution
# - we generate all possible orders of ad companies (order how add is being shown to users)
# - then we execute calculations based on both approaches for each order

## generate all possible combinations for orders
all_company_orders.mat <- permutations(n = nr_comp,
                                       r = nr_comp,
                                       v = 1:nr_comp)

## number of all orders-combinations
nr_orders <- nrow(all_company_orders.mat)

## run estimations (also simulation) 
set.seed(1123)

df.rez <- map_dfr(seq_len(nr_orders), function(row_i){
  
  # selected orders of ad companies
  order <- all_company_orders.mat[row_i, ]
  
  # execute both functions (theoretical & simulation expected revenue)
  # - for selected companies order
  # - we store:
  #   - company order sequence
  #   - expected user revenue (theoretical calculation)
  #   - expected user revenue (based on MC simulation)
  #   - difference between each expected user revenue
  #   - add ranks - best for each revenue
  tibble(order_ids      = paste(df.comp$id[order], collapse = " > "),
         ER_theory      = theor_exp_rev_order(order_idx_ = order,
                                              prob_      = comp_prob_fill, 
                                              rev_       = comp_rev_exp),
         ER_emp_sim_MC  = sim_exp_rev_order(order_idx_   = order, 
                                            prob_        = comp_prob_fill, 
                                            rev          = comp_rev_exp,
                                            n_users_     = n_users, 
                                            n_reps_      = n_reps
                                            ))}) %>%
  mutate(diff_emp_sim_theory = ER_emp_sim_MC - ER_theory) %>%
  # add ER theory rank
  arrange(desc(ER_theory)) %>% 
  mutate(ER_theory_rank = row_number()) %>% 
  # add ER empirical - simulated rank
  arrange(desc(ER_emp_sim_MC)) %>% 
  mutate(ER_emp_sim_rank = row_number())



# Determine best order based on smart sort (revenue sort)
# - this step covers estimation of expected revenue based on approach 3
# - in this setup (show first available ad, no per-query costs or penalties),
# - the optimal order is simply sorting companies by descending revenue per impression (r).
# - we add solution obtained from approach 3 to main results table
df.rez <- add_smart_sort_best_order()



# Show results
# - print best company order for each approach and show expected revenue per user
# - visualize distribution of expected user revenues (approaches 1 and 2) 
# - and add final result from approach 3 to the mix
show_final_results(); export_fig("01_expected_revenue_user_distribution.png", path_fig)

