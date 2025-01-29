

many_results %>%
  dplyr::select(wilcox:coinasymp, sample_size) %>%
  pivot_longer(cols = -sample_size, names_to = "test_name") %>%
  mutate(test_name = stats::reorder(.data[["test_name"]], .data[["value"]], decreasing = TRUE)) %>%
  ggplot(aes(sample_size, value, color = test_name)) +
    geom_smooth(alpha = 0.1) +
    theme_bw()



load_all()

many_results %>%
  dplyr::select(wilcox:coinasymp, sample_size) %>%
  plot_distribution_results()




load_all()

many_results %>%
  dplyr::select(wilcox:coinasymp, sample_size) %>%
  calculate_power_t2error() %>%
  ggplot(aes(x = `Sample Size`, y = power,
             ymin = lower_power_bound, ymax = upper_power_bound,
             color = test, fill = test)) +
  geom_smooth(method="glm",
              method.args=list(family="binomial"),
              se = F) +
  # geom_smooth()
  # geom_ribbon(alpha = 0.1, linetype = 2) +
  theme_bw()





""


test_t1_error %>%
  dplyr::select(wilcox:coinasymp, sample_size) %>%
  group_by(sample_size) %>%
  calculate_t1_error()

