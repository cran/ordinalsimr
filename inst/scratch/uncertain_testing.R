

many_results <- run_simulations(sample_size = c(20:100),
                sample_prob = c(0.5,0.5),
                prob0 = c(0.1,0.2,0.3,0.4),
                prob1 = c(0.6,0.2,0.1,0.1),
                niter = 20) %>%
  bind_rows()



hello <- many_results %>%
  select(sample_size, wilcox:coinasymp) %>%
  pivot_longer(wilcox:coinasymp, names_to = "tests", values_to = "t1_error")

hello %>%
  ggplot(aes(sample_size, t1_error, color = tests)) +
  geom_smooth(alpha = 0.1) +
  theme_bw()


many_results %>%
  dplyr::select(wilcox:coinasymp) %>%
  plot_distribution_results()
