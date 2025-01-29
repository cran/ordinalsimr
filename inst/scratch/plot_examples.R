


ordinalsimr::simulation_data_two_groups %>%
  dplyr::select(
    dplyr::any_of(c(
      "Wilcoxon", "Fisher", "Chi Squared\n(No Correction)",
      "Chi Squared\n(Correction)", "Prop. Odds", "Coin Indep. Test"
    )),
    .data$sample_size
  ) %>%
  plot_distribution_results()




ordinalsimr::simulation_data_two_groups %>%
  select(
    dplyr::any_of(c(
      "Wilcoxon", "Fisher", "Chi Squared\n(No Correction)",
      "Chi Squared\n(Correction)", "Prop. Odds", "Coin Indep. Test"
    )),
    .data$sample_size
  ) %>%
  calculate_power_t2error() %>%
  plot_power()
