

# data_for_violin <<- df
# saveRDS(data_for_violin, file = "inst/scratch/data_for_violin.rds")


data_for_violin <- readRDS("inst/scratch/data_for_violin.rds")


levels <- data_for_violin %>%
  pivot_longer(cols = -all_of("sample_size"), names_to = "test_name") %>%
  group_by(.data[["test_name"]]) %>%
  summarize(
    mean = mean(.data[["value"]], na.rm = T)
  ) %>%
  arrange(.data[["mean"]]) %>%
  pull(.data[["test_name"]])

unique_sample_sizes <- data_for_violin %>%
  pull(.data[["sample_size"]]) %>%
  unique()
sample_sizes_length <- length(unique_sample_sizes)
if (sample_sizes_length < 8) {
  indices <- seq(1, sample_sizes_length)
} else {
  # Select 8 evenly spaced sample sizes
  indices <- round(seq(1, sample_sizes_length, length.out = 8))
}
selected_sample_sizes <- unique_sample_sizes[indices]
label_text <- c(paste("p \u2265", 0.05), paste("p \u003C", 1-0.05))





data_for_violin %>%
  pivot_longer(cols = -all_of("sample_size"), names_to = "test_name") %>%
  mutate(test_name = factor(.data$test_name, levels = levels)) %>%
  dplyr::filter(.data[["sample_size"]] %in% selected_sample_sizes) %>%
  ggplot(aes(x = factor(.data[["sample_size"]]), y = .data[["value"]], fill = factor(.data[["sample_size"]]))) +
  geom_boxplot(alpha = 0.5, color = NA) +
  labs(
    title = "P-value Distributions Across Sample Sizes",
    x = "P-value",
    y = "Sample Size"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 18),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12)
  ) +
  facet_wrap(~ .data[["test_name"]], scales = "free_y") +
  coord_flip()

