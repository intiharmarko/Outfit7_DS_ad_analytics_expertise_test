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



#' Show results: for expected user revenue & optimal ad company sorting
# - print best company order for each approach and show expected revenue per user
# - visualize distribution of expected user revenues (approaches 1 and 2) 
# - and add final result from approach 3 to the mix
#'   
#' @param df.rez_   A data frame - which includes already expected revenue estimation based on simulation and theoretical approach.
#'
#' @return None
#'
show_final_results <- function(df.rez_ = df.rez){
  
  # extract optimal order and expected revenue per user
  # - for each approach
  # - for smart sort we use values from theoretical expected revenues per users
  a1_order <- df.rez_ %>% filter(ER_emp_sim_rank == 1) %>% pull(order_ids)
  a1_ER    <- df.rez_ %>% filter(ER_emp_sim_rank == 1) %>% pull(ER_emp_sim_MC)
  
  a2_order <- df.rez_ %>% filter(ER_theory_rank == 1) %>% pull(order_ids)
  a2_ER    <- df.rez_ %>% filter(ER_theory_rank == 1) %>% pull(ER_theory)
  
  a3_order <- df.rez_ %>% filter(ER_smart_sort_best == 1) %>% pull(order_ids)
  a3_ER    <- df.rez_ %>% filter(ER_smart_sort_best == 1) %>% pull(ER_theory)
  
  
  # report results
  cat("\n--------------------------------------------------------------------------------------------")
  cat("\n--------------------------------------------------------------------------------------------")
  cat(paste0("\nBest ad company order by selected approach!"))
  cat("\n--------------------------------------------------------------------------------------------")
  cat("\n--------------------------------------------------------------------------------------------")
  
  cat("\n ")
  cat("\n--------------------------------------------------------------------------------------------")
  cat(paste0("\nApproach 1: Empirical estimation based on MC simulation (users = ", n_users, " reps = ", n_reps, ")"))
  cat(paste0("\nAd company order: ", a1_order))
  cat(paste0("\nExpected revenue per user: ", a1_ER))
  
  cat("\n ")
  cat("\n--------------------------------------------------------------------------------------------")
  cat("\nApproach 2: Theoretical estimation based on probability distribution")
  cat(paste0("\nAd company order: ", a2_order))
  cat(paste0("\nExpected revenue per user: ", a2_ER))
  
  cat("\n ")
  cat("\n--------------------------------------------------------------------------------------------")
  cat("\nApproach 3: Order determined based on smart sort (revenue sort).")
  cat(paste0("\nAd company order: ", a3_order))
  cat(paste0("\nExpected revenue per user: ", a3_ER, " (value used from theoretical expected revenues estimations!)"))
  
  
  # visualize results
  # - show distributions of expected revenues for users for every possible ordering
  # - for approaches 1 and 2
  # - mark selected final company orders
  # - also mark selected final company order based on approach 3
  p1 <- df.rez_ %>% 
    ggplot(aes(x = ER_emp_sim_MC)) +
    geom_density(color = "black",
                 fill = "deepskyblue4",
                 alpha = 0.1) +
    geom_vline(xintercept = a1_ER,
               color = "deepskyblue4", 
               linewidth = 1.2,
               linetype = "dashed") +
    annotate(geom = "text",
             x = a1_ER,
             y = 1,
             color = "deepskyblue4",
             label = paste0("MC sim - ER: ", round(a1_ER, 3)),
             size = 4) +
    annotate(geom = "text",
             x = a1_ER,
             y = 0.90,
             color = "deepskyblue4",
             label = paste0("", a1_order),
             size = 4) +
    geom_vline(xintercept = a3_ER,
               color = "brown4", 
               linewidth = 1.2,
               linetype = "dotted") +
    annotate(geom = "text",
             x = a3_ER,
             y = 0.5,
             color = "brown4",
             label = paste0("Smart sort - ER: ", round(a3_ER, 3)),
             size = 4) +
    annotate(geom = "text",
             x = a3_ER,
             y = 0.40,
             color = "brown4",
             label = paste0("", a3_order),
             size = 4) +
    scale_x_continuous(breaks = seq(0, 10, 0.1),
                       limits = c(3, 5)) +
    xlab("Expected revenue for user (in USD)") +
    ylab("Density") +
    ggtitle("Distribution of expected revenues - Empirical values based on MC simulation") +
    labs(subtitle = paste0("All possible company orders tested (", nr_orders, " different orders)\nSimulation setting (users = ", n_users, ", reps = ", n_reps,")")) +
    theme_minimal(base_size = 14)
  
  p2 <- df.rez_ %>% 
    ggplot(aes(x = ER_theory)) +
    geom_density(color = "black",
                 fill = "gray20",
                 alpha = 0.1) +  
    geom_vline(xintercept = a2_ER,
               color = "gray20", 
               linewidth = 1.2,
               linetype = "solid") +
    annotate(geom = "text",
             x = a2_ER,
             y = 1,
             color = "gray20",
             label = paste0("Theoret. - ER: ", round(a2_ER, 3)),
             size = 4) +
    annotate(geom = "text",
             x = a2_ER,
             y = 0.90,
             color = "gray20",
             label = paste0("", a2_order),
             size = 4) +
    geom_vline(xintercept = a3_ER,
               color = "brown4", 
               linewidth = 1.2,
               linetype = "dotted") +
    annotate(geom = "text",
             x = a3_ER,
             y = 0.5,
             color = "brown4",
             label = paste0("Smart sort - ER: ", round(a3_ER, 3)),
             size = 4) +
    annotate(geom = "text",
             x = a3_ER,
             y = 0.40,
             color = "brown4",
             label = paste0("", a3_order),
             size = 4) +
    scale_x_continuous(breaks = seq(0, 10, 0.1),
                       limits = c(3, 5)) +
    
    xlab("Expected revenue for user (in USD)") +
    ylab("Density") +
    ggtitle("Distribution of expected revenues - Theoretical values based on probabilities") +
    labs(subtitle = paste0("All possible company orders tested (", nr_orders, " different orders)")) +
    theme_minimal(base_size = 14)
  
  # combine both figures and show the final plot
  p1 / p2
}
