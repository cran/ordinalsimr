## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ordinalsimr)

## ----warning=FALSE------------------------------------------------------------
sim_results <- run_simulations(
  sample_size = 80,
  sample_prob = c(0.5, 0.5),
  prob0 = c(0.1, 0.2, 0.3, 0.4),
  prob1 = c(0.6, 0.2, 0.1, 0.1),
  niter = 20
)

formatted_results <- dplyr::bind_rows(sim_results)
names(formatted_results)

formatted_results %>%
  dplyr::select(
    Wilcoxon, Fisher, `Chi Squared (No Correction)`,
    `Chi Squared (Correction)`, `Prop. Odds`,
    `Coin Indep. Test`,
    sample_size
  ) %>%
  calculate_power_t2error(alpha = 0.05, power_confidence_int = 95)

## ----warning=FALSE------------------------------------------------------------
sim_results <- run_simulations(
  sample_size = 30:35,
  sample_prob = c(0.5, 0.5),
  prob0 = c(.4, .3, .3),
  prob1 = c(.8, .1, .1), # note the matching probabilities between groups
  niter = 50
)

formatted_results <- dplyr::bind_rows(sim_results)
names(formatted_results)


formatted_results %>%
  dplyr::select(
    Wilcoxon, Fisher, `Chi Squared (No Correction)`,
    `Chi Squared (Correction)`, `Prop. Odds`,
    `Coin Indep. Test`,
    sample_size
  ) %>%
  calculate_t1_error(alpha = 0.05, t1_error_confidence_int = 95)

## ----warning=FALSE------------------------------------------------------------
# Create a vector of sample sizes
sample_sizes <- c(30, 50, 100)

# Map over the sample sizes
lapply(sample_sizes, function(x) {
  run_simulations(
    sample_size = x,
    sample_prob = c(0.5, 0.5),
    prob0 = c(0.1, 0.2, 0.3, 0.4),
    prob1 = c(0.6, 0.2, 0.1, 0.1),
    niter = 100
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::select(
      Wilcoxon, Fisher, `Chi Squared (No Correction)`,
      `Chi Squared (Correction)`, `Prop. Odds`,
      `Coin Indep. Test`, sample_size
    ) %>%
    calculate_power_t2error()
})

## ----warning=FALSE------------------------------------------------------------
# Choose sample sizes
sample_size <- c(50, 100)
# Set sample distributions as a proportion c(group1, group2)
sample_prob <- list(c(0.5, 0.5), c(0.75, 0.25))
# Trial 1 has matching probabilities between the 2 groups. Trial 2 has non-matching probabilities
prob0_list <- list(trial1_group1 = c(0.1, 0.2, 0.3, 0.4), trial2_group1 = c(0.1, 0.2, 0.3, 0.4))
prob1_list <- list(trial1_group2 = c(0.1, 0.2, 0.3, 0.4), trial2_group2 = c(0.2, 0.3, 0.3, 0.2))
# Number of iterations
niter <- c(20, 100)

## ----warning=FALSE------------------------------------------------------------
# Use tidyr::expand_grid as it creates a tibble, supporting the nested tibble structure
info_table <- tidyr::expand_grid(
  sample_size,
  sample_prob,
  prob0_list,
  prob1_list,
  niter
)

info_table

## ----warning=FALSE------------------------------------------------------------
# Calculate either Power/T2 error or T1 error depending on your specific needs
many_sims <- mapply(
  ordinalsimr::run_simulations,
  sample_size = info_table$sample_size,
  sample_prob = info_table$sample_prob,
  prob0 = info_table$prob0_list,
  prob1 = info_table$prob1_list,
  niter = info_table$niter
)


length(many_sims)
many_sims[1]

