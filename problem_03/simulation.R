# Problem 3: Ad-Mediation Optimization
#
# - goal: Find optimal sorting order for ads providers companies (based on expected user revenue).
# - we will demonstrate three different approaches
#   - empirical: MC simulation (all combinations)
#   - theoretical estimation (all combinations)
#   - smart approach (efficient sorting of companies)

rm(list = ls())
graphics.off()


# Libraries
library(tidyverse)
library(gtools)

# Load custom functions
source("./problem_03/funct_sim.R")


# Paths
path_fig  <- "./problem_03/fig/"


# Global variables (inputs)
comp_prob_fill <-  c(0.10, 0.19, 0.40, 0.45, 0.50) # fill rate (probability) - ad companies
comp_rev_exp   <- c(10,  5,  4,  3,  5)            # actual expected revenue - ad companies
nr_comp        <- length(comp_prob_fill)           # number of ad provider companies 
n_users        <- 100                             # number of users (simulation parameter)
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


# Determine best order based on smart sort
# - smart sort logic:
#   - in this setup (no per-query cost, ad shown immediately on first success),
#   - the optimal order is simply to sort companies by revenue per impression (r) in descending order.
#   - proof: comparing two companies i and j, the difference in expected revenue depends only on (r_i - r_j),
#   - so higher revenue must always come first, regardless of fill rate

add_smart_sort_best_order <- function(df.comp_ = df.comp,
                                      )


df.comp <- df.comp %>%
  mutate(score = (p * r) / (1 - p))

smart_ord <- df.comp %>%
  arrange(desc(score)) %>%
  pull(id)

smart_sort <- paste(smart_ord, collapse = " > ")


df.rez <- df.rez %>% 
  mutate(ER_smart_sort_best = if_else(order_ids == smart_sort, 1, 0))






library(patchwork)

p1 <- df.rez %>% 
  ggplot(aes(x = ER_emp_sim_MC)) +
  geom_density()

p2 <- df.rez %>% 
  ggplot(aes(x = ER_theory)) +
  geom_density()

p1 / p2
