


ordinalsimr::simulation_data_two_groups %>%
  dplyr::select(wilcox:coinasymp, sample_size) %>%
  calculate_power_t2error(
    alpha = .05,
    n = 200,
    power_confidence_int = 95
  ) %>%
  ggplot(aes(
    x = .data[["Sample Size"]], y = .data[["power"]],
    ymin = .data[["lower_power_bound"]], ymax = .data[["upper_power_bound"]],
    color = .data[["test"]], fill = .data[["test"]]
  )) +
  geom_line() +
  theme_bw() +
  theme(
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 18),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
  )



ordinalsimr::simulation_data_two_groups %>%
  dplyr::select(wilcox:coinasymp, sample_size) %>%
  calculate_power_t2error(
    alpha = .05,
    n = 200,
    power_confidence_int = 95
  ) %>%
  plot_power()
