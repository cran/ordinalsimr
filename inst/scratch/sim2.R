

devtools::load_all()

probability <- c(0.044, 0.063, 0.113, 0.016, 0.075, 0.028, 0.022, 0.39, 0.249)
worst_case <- c(0.044, 0.065, 0.115, 0.016, 0.076, 0.035, 0.027, 0.41, 0.212)
best_case <- c(0.04, 0.055, 0.1, 0.014, 0.07, 0.02, 0.018, 0.35, 0.333)


check2 <- run_simulations(sample_size = c(30:300),
                          sample_prob = c(0.5,0.5),
                          prob0 = probability,
                          prob1 = worst_case,
                          niter = 1000) %>%
  bind_rows()

neo_results2 <- check2 %>%
  select(sample_size, wilcox:coinasymp) %>%
  pivot_longer(wilcox:coinasymp, names_to = "tests", values_to = "p_values")

sim2_results$neo_results2 %>%
  ggplot(aes(sample_size, t1_error, color = tests)) +
  geom_smooth(alpha = 0.1) +
  theme_bw() +
  ggplot2::labs(y = "p-values")
