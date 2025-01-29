


# load_all()
#
# ordinalsimr::simulation_data_two_groups %>%
#   dplyr::select(wilcox:coinasymp, sample_size) %>%
#   plot_distribution_results()


ordinalsimr::simulation_data_two_groups  %>%
  group_by(.data[["sample_size"]]) %>%
  select(wilcox:coinasymp) %>%
  pivot_longer(cols = -sample_size, names_to = "test_name") %>%
  group_by(sample_size, test_name) %>%
  summarise(mean_p = mean(value))



alpha <- 0.05
power_confidence_int <- 95

ordinalsimr::simulation_data_two_groups  %>%
    group_by(.data[["sample_size"]]) %>%
    select(wilcox:coinasymp) %>%
    dplyr::group_modify(
      ~ {
        purrr::map(.x, ~ {
          binom_power <- binom.test(sum(.x < alpha), length(.x), conf.level = power_confidence_int / 100)
          tibble(
            lower_power_bound = binom_power$conf.int[[1]],
            upper_power_bound = binom_power$conf.int[[2]],
            p_value = binom_power$estimate
          )
        }) %>%
          purrr::list_rbind(names_to = "test")
      }
    ) %>%
  ggplot(., aes(x = .data[["sample_size"]], y = .data[["value"]], color = .data[["test"]])) +
  geom_line() +
  geom_hline(yintercept = .05, linetype = "dashed", linewidth = 2) +
  ggtitle("Plot of p-values") +
  labs(x = "Sample Size", y = "p-value", color = "Statistical Test") +
  guides(fill = "none") +
  theme_bw() +
  theme(
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 18),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
  )




ordinalsimr::simulation_data_two_groups %>%
  dplyr::select(wilcox:coinasymp, sample_size) %>%
  pivot_longer(cols = -.data$sample_size, names_to = "test_name") %>%
  mutate(test_name = stats::reorder(.data[["test_name"]], .data[["value"]], decreasing = TRUE)) %>%
  group_by(sample_size, test_name) %>%
  summarise(value = mean(value)) %>%
    ggplot(., aes(x = .data[["sample_size"]], y = .data[["value"]], color = .data[["test_name"]])) +
      geom_line() +
      geom_hline(yintercept = .05, linetype = "dashed", linewidth = 2) +
      ggtitle("Plot of p-values") +
      labs(x = "Sample Size", y = "p-value", color = "Statistical Test") +
      guides(fill = "none") +
      theme_bw() +
      theme(
        axis.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 18),
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
      )



ordinalsimr::simulation_data_two_groups %>%
  dplyr::select(wilcox:coinasymp, sample_size) %>%
  plot_distribution_results()

