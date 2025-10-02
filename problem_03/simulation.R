# Problem 3: Ad-Mediation Optimization
#
# - goal: Find optimal sorting order for ads providers companies.
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
prob_fill    <-  c(0.10, 0.19, 0.40, 0.45, 0.50) # fill rate (probability) - ad companies
rev_exp      <- c(10,  5,  4,  3,  5)            # actual expected revenue - ad companies
nr_companies <- length(prob_fill)                # number of ad provider companies 
n_users      <- 100                              # number of users (simulation parameter)
n_reps       <- 100                              # number of repetitions (simulation parameter)


df.companies <- tibble(id = paste0("C", 1:nr_companies),
                       p  = prob_fill,  
                       r  = rev_exp)



# Approach 1: Monte Carlo (MC) simulation
#
# - we will check all possible company orders (combinations)
# - for each order (combination):
#   - we will simulate how ads would be shown to selected population of users
#   - consider order, probability that ad is shown and expected revenue
#   - assign revenues in the order given in the iteration
#   - estimate average expected revenue over all users (after assignment)
#   - repeat this simulation for as many repetitions you have selected
#   - calculate average expected revenues over all repetitions
# - at the end select order with highest expected user revenue over all combinations

sim_exp_rev_single_order <- function(order_idx, p, r, n_users, n_reps){

  # initialize vector for user assigned revenue
  rev_user <- numeric(n_reps)
  
  # iterate over all repetition
  for (rep in seq_len(n_reps)){
    
    # initialize remaining users and total sum for revenue per user
    remaining <- n_users
    rev_sum   <- 0
    
    # iterate over each ad company in selected company order (combination)
    for (i in order_idx){
      
      # stop if no user remains (all were served with ads)
      if (remaining == 0L) {
        break
      } 
      
      # select number of users served with given ad company
      served_i <- rbinom(1L, size = remaining, prob = p[i])
      
      # if at least one user is served
      if (served_i > 0L){
        
        # calculate running total for user revenues
        rev_sum  <- rev_sum + served_i * r[i]
        
        # reduce number of remaining users
        remaining <- remaining - served_i
      }
    }
    
    # current expected user revenue
    rev_user[rep] <- rev_sum / n_users
  }
  
  # average expected revenue per user over all repetitions
  rev_user_exp <- mean(rev_user)
  
  return(rev_user_exp)
}

# all possible combinations for orders

all_company_orders.mat <- permutations(n = nr_companies, 
                                       r = nr_companies, 
                                       v = 1:nr_companies)


nr_orders <- nrow(all_company_orders.mat)

# run simulation for all orders
rev_user_exp_order <- numeric(nr_orders)

for (i in seq_len(nr_orders)){
  
  # extract company orders
  order_idx_ <- all_company_orders.mat[i,]
  
  # simulate expected user revenue for selected order
  rev_user_exp <- sim_exp_rev_single_order(order_idx_, prob_fill, rev_exp, n_users, n_reps)
  
  # store results
  rev_user_exp_order[i] <- rev_user_exp
}


df.rez <- as_data_frame(all_company_orders.mat) %>% 
  rename(`company 1st (id)` = V1, 
         `company 2nd (id)` = V2, 
         `company 3rd (id)` = V3, 
         `company 4th (id)` = V4,
         `company 5th (id)` = V5) %>% 
  mutate(`expected user revenue (sim)` = rev_user_exp_order)


expected_revenue_order <- function(order_idx, p, r){
  cum_fail <- 1
  er <- 0
  for(k in seq_along(order_idx)){
    i <- order_idx[k]
    er <- er + cum_fail * p[i] * r[i]
    cum_fail <- cum_fail * (1 - p[i])
  }
  er
}


df.rez <- map_dfr(seq_len(nr_orders), function(row_i){
  ord <- all_company_orders.mat[row_i, ]
  tibble(
    order_ids  = paste(companies$id[ord], collapse = " > "),
    ER_theory  = expected_revenue_order(ord, p, r),
    ER_emp_MC  = simulate_order_fast(ord, p, r, n_users, n_reps)
  )
}) %>%
  mutate(diff_emp_theory = ER_emp_MC - ER_theory) %>%
  arrange(desc(ER_emp_MC))





