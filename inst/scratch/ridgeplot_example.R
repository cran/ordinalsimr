sample_sizes <- seq(20, 200, by = 20)  # 10 sample sizes
iterations <- 1000

# Simulate p-values: under null hypothesis (uniform), and gradually shift toward low p-values
simulated_data <- do.call(rbind, lapply(sample_sizes, function(n) {
  # Simulate "effect" — smaller p-values with larger sample size
  effect_strength <- (n - min(sample_sizes)) / (max(sample_sizes) - min(sample_sizes))  # from 0 to 1
  shape1 <- 1 - effect_strength + 0.5  # skews more toward 0 as n increases
  shape2 <- 1 + effect_strength * 10

  data.frame(
    sample_size = n,
    p_value = rbeta(iterations, shape1, shape2),  # Use Beta to mimic skewed p-values
    test = "Test_A"
  )
}))

simulated_data <- bind_rows(simulated_data,
      do.call(rbind, lapply(sample_sizes, function(n) {
        # Simulate "effect" — smaller p-values with larger sample size
        effect_strength <- (n - min(sample_sizes)) / (max(sample_sizes) - min(sample_sizes))  # from 0 to 1
        shape1 <- 1 - effect_strength + 0.5  # skews more toward 0 as n increases
        shape2 <- 1 + effect_strength * 10

        data.frame(
          sample_size = n,
          p_value = rbeta(iterations, shape1, shape2),  # Use Beta to mimic skewed p-values
          test = "Test_B"
        )
      }))
)


# install.packages("ggridges")
# library(ggridges)

# df %>%
#   pivot_longer(cols = -all_of("sample_size"), names_to = "test_name") %>%
#   mutate(test_name = factor(.data$test_name, levels = levels))

levels <- temp_data %>%
  pivot_longer(cols = -all_of("sample_size"), names_to = "test_name") %>%
  group_by(.data[["test_name"]]) %>%
  summarize(
    mean = mean(.data[["value"]], na.rm = T)
  ) %>%
  arrange(.data[["mean"]]) %>%
  pull(.data[["test_name"]])


# temp_data

unique_sample_sizes <- temp_data %>%
  pull(sample_size) %>%
  unique()

sample_sizes_length <- length(unique_sample_sizes)

if (sample_sizes_length < 8) {
  indices <- seq(1, sample_sizes_length)
} else {
  # Select 8 evenly spaced sample sizes
  indices <- round(seq(1, sample_sizes_length, length.out = 8))
}

indices <- round(seq(1, sample_sizes_length, length.out = 8))
selected_sample_sizes <- unique_sample_sizes[indices]


temp_data %>%
  pivot_longer(cols = -all_of("sample_size"), names_to = "test_name") %>%
  mutate(test_name = factor(.data$test_name, levels = levels)) %>%
  dplyr::filter(sample_size %in% selected_sample_sizes) %>%
  ggplot(aes(x = value, y = factor(sample_size), fill = after_stat(.data[["x"]] < 0.05))) +
    geom_density_ridges_gradient(
      scale = 3,
      rel_min_height = 0.01,
      quantile_lines = TRUE, quantiles = 2,
      alpha = 0.3
      ) +
    scale_fill_manual(values = c("TRUE" = "#FF00004D", "FALSE" = "#87CEEB4D"),
                      name = "Significance",
                      labels = c("p ≥ 0.05", "p < 0.05")) +
    labs(
      title = "P-value Distributions Across Sample Sizes",
      x = "P-value",
      y = "Sample Size"
    ) +
    theme_minimal() +
    theme(legend.position = "top") +
    facet_wrap(~ test_name, ncol = 2)
