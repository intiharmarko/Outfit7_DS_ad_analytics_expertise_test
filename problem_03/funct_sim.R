#' Helper Functions for simulation


library(tidyverse)
library(gtools)
library(patchwork)



#' Simulate app querying process - Monte Carlo (MC) simulation
#' - and estimate empirical expected user revenue (based ons simualtion)
#'
#' - select given ad companies orders (combinations)
#' - for given order (combination):
#'   - we will simulate how ads would be shown to selected population of users
#'   - consider order, probability that ad is shown and expected revenue
#'   - assign revenues in the order given in the iteration
#'   - estimate average expected revenue over all users (after assignment)
#'   - repeat this simulation for as many repetitions you have selected
#'   - calculate average expected revenues over all repetitions
#'   
#' @param order_idx_ An integer vector - order of ad companies (company ids).
#' @param prob_      A float vector - each company fill rate (probability ad is shown).
#' @param rev_       A float vector - expected revenue per each user for given ad company.
#' @param n_users_   An integer - number of users used in simulation.
#' @param n_reps_    An integer - number of repetitions (iterations) of the simulation. 
#'
#' @return rev_user_rez_avg A float - expected user revenue calculated as average over all repetitions.
#'
sim_exp_rev_order <- function(order_idx_, 
                              prob_, 
                              rev_, 
                              n_users_, 
                              n_reps_){
  
  # initialize vector for user assigned revenue (result of each iteration)
  rev_user_rez <- numeric(n_reps_)
  
  # iterate over all repetition
  for (rep in seq_len(n_reps_)){
    
    # initialize remaining users and total sum for revenue per user
    nr_remaining <- n_users_
    rev_sum      <- 0
    
    # iterate over each ad company in selected company order (combination)
    for (i in order_idx_){
      
      # stop if no user remains (all were served with ads)
      if (nr_remaining == 0L) {
        break
      } 
      
      # select number of users served with given ad company
      nr_served_i <- rbinom(1L, 
                            size = nr_remaining, 
                            prob = prob_[i])
      
      # if at least one user is served
      if (nr_served_i > 0L){
        
        # calculate running total for user revenues
        rev_sum  <- rev_sum + nr_served_i * rev_[i]
        
        # reduce number of remaining users
        nr_remaining <- nr_remaining - nr_served_i
      }
    }
    
    # current expected user revenue
    rev_user_rez[rep] <- rev_sum / n_users_
  }
  
  # average expected revenue per user over all repetitions
  rev_user_rez_avg <- mean(rev_user_rez)
  
  return(rev_user_rez_avg)
}




#' Theoretical expected user revenue
#' - select given ad companies orders (combinations)
#' - for given order (combination):
#'   - we will simulate how ads would be shown to selected population of users
#'   - consider order, probability that ad is shown and expected revenue
#'   - assign revenues in the order given in the iteration
#'   - estimate average expected revenue over all users (after assignment)
#'   - repeat this simulation for as many repetitions you have selected
#'   - calculate average expected revenues over all repetitions
#'   
#' @param order_idx_ An integer vector - order of ad companies (company ids).
#' @param prob_      A float vector - each company fill rate (probability ad is shown).
#' @param rev_       A float vector - expected revenue per each user for given ad company.
#'
#' @return rev_sum A float - expected user revenue calculated as based on theoretical distribution.
#'

theor_exp_rev_order <- function(order_idx_, 
                                prob_, 
                                rev_){

  # initialize
  cum_fail_prob <- 1 # probability that all previous companies have failed so far
  rev_sum <- 0       # running total for expected revenue

  for(k in seq_along(order_idx_)){

    # actual company index at position k
    i <- order_idx_[k]

    # add this company's contribution:
    #   P(all previous failed) * P(this one succeeds) * revenue if succeeds
    rev_sum <- rev_sum + cum_fail_prob * prob_[i] * rev_[i]

    # update probability that user still has no ad after this company
    cum_fail_prob <- cum_fail_prob * (1 - prob_[i])
  }

  return(rev_sum)
}



#' Determine best order based on smart sort (revenue sort)
#' - smart sort logic:
#'   - in this setup (show first available ad, no per-query costs or penalties),
#'   - the optimal order is simply sorting companies by descending revenue per impression (r).
#'   - proof: comparing any two companies i and j, the expected revenue difference depends only on (r_i - r_j),
#'   - so higher-revenue networks must always be placed earlier, regardless of their fill rates.
#'   
#' @param df.comp_  A data frame - data about add companies (ids, fill rates, revenues per impression).
#' @param df.rez_   A data frame - which includes already expected revenue estimation based on simulation and theoretical approach.
#'
#' @return df.rez_ A data frame - updated with smart sort results.
#'
add_smart_sort_best_order <- function(df.comp_ = df.comp,
                                      df.rez_ = df.rez){
  
  # sort ad companies based on expected revenue per impression
  # - in descending order (add second order level fill rate also in descending order)
  # - extract companies ids
  # - and create company order as string
  smart_order <- df.comp_ %>%
    arrange(desc(r), desc(p)) %>%
    pull(id) %>% 
    paste(., collapse = " > ")
  
  # add smart sort solution to the final results table
  # - first we match selected order in the table
  # - and we add flag, which order was selected
  df.rez_ <- df.rez_ %>% 
    mutate(ER_smart_sort_best = if_else(order_ids == smart_order, 1, 0))
  
  return(df.rez_)
}
  