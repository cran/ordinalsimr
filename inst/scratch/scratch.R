

assignments <- assign_groups(200, c(0.5,0.5), c(0.1,0.2,0.3,0.4), c(0.2,0.4,0.3,0.1), 1)


debug(run_simulations)
# undebug(run_simulations)
run_simulations(sample_size = 80,
                sample_prob = c(0.5,0.5),
                prob0 = c(0.1,0.2,0.3,0.4),
                prob1 = c(0.6,0.2,0.1,0.1),
                niter = 20
                )[[1]]



sim_vals <- run_simulations(sample_size = 100,
                            sample_prob = c(0.5,0.5),
                prob0 = c(0.1,0.2,0.3,0.4),
                # prob0 = c(0.2,0.4,0.3,0.1),
                prob1 = c(0.6,0.2,0.1,0.1),
                niter = 30
                )[[1]]

sim_vals %>%
  as.data.frame() %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "test_name") %>%
  # pull(value) %>% mean()
  dplyr::slice_min(order_by = value, prop = .9) %>%
  ggplot(aes(x = value, fill = test_name)) +
  geom_density(alpha = 0.5, color = "black") +
  theme_bw()



pwr.chisq.test(w = 0.1, N = 300, df = 13, sig.level = 0.05) %>%
  str()

pwr.chisq.test(w = 0.2, N = 300, df = 13, sig.level = 0.05)$power

# # wilcox, fisher, chisq, lrm, kruskal, coin asymp.
#
# sim_vals %>%
#   as.data.frame() %>%
#   # pull(chisqTRUE) %>%
#   mutate(
#     wilcox_power = NA,
#     fisher_power = NA,
#     chisqFALSE_power = pwr.chisq.test(w = 0.8, N = 60, df = 13, sig.level = chisqFALSE)[["power"]],
#     chisqTRUE_power = pwr.chisq.test(w = 0.8, N = 60, df = 13, sig.level = chisqTRUE)[["power"]],
#     lrm_power = NA,
#     kruskal_power = NA,
#     coinasymp_power = NA
#          )
#
#
#   purrr::map_dbl( ~
#                     c(pwr.chisq.test(w = 0.8, N = 60, df = 13, sig.level = .x)[["power"]]
#                     )
#                   ) %>%
#   tibble(powers = .) %>%
#   ggplot(aes(x = powers)) +
#   geom_density()




sim_vals %>%
  as.data.frame() %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "test_name") %>%
  mutate(greater_than_threshold = value < 0.01) %>%
  group_by(test_name) %>%
  summarize("avg p-value" = mean(value),
            "percent below threshold" = mean(greater_than_threshold),

            )








### T1 error, T2 error, and power calculations

# need to confirm that these calculations are correct

alpha <- 0.00001
# sim_vals %>%
#   as.data.frame() %>%
#   tibble() %>%
#   pull(wilcox) %>%
#   {mean(. <= alpha)}

# T1 error for all cols
sim_vals %>%
  as.data.frame() %>%
  tibble() %>%
  summarize(
    across(everything(),
           list(t1_error = ~mean(.x),
                power = ~mean(.x < alpha),
                t2_error = ~1-mean(.x < alpha)
                ),
           .names = "{.col}_{.fn}"
           )
  ) %>%
  # can't figure out how to format this correctly just off
  # pivot longer so need to separate and then pivot wider
  # again to get 3 separate columns for results
  pivot_longer(cols = everything(),
               names_to = "test",
               values_to = "value") %>%
  separate(
    col = test,
    into = c("name", "statistic"),
    sep = "_",
    extra = "merge",
    remove = F
  ) %>%
  select(-test) %>%
  pivot_wider(
    names_from = statistic,
    names_glue = "{statistic}_{.value}",
    values_from = value
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 5)))



# names_pattern = "(.*)_t1_(error|power)|(.*)_power"




# Calculate power confidence interval

z_score <- qnorm((100+99)/200)
p <-mean(sim_vals[,1] < alpha)

p + z_score*sqrt(p*(1-p))/length(sim_vals[,1])
p - z_score*sqrt(p*(1-p))/length(sim_vals[,1])



# Calculate T1 Error


sim_vals_null <- run_simulations(sample_size = 100,
                            sample_prob = c(0.5,0.5),
                            prob0 = c(0.1,0.2,0.3,0.4),
                            prob1 = c(0.1,0.2,0.3,0.4),
                            niter = 30)[[1]]


z_score <- qnorm((100+95)/200)
alpha <- 0.05
sim_vals_null %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = "test", values_to = "value") %>%
  group_by(test) %>%
  summarize(lower_t1_bound = mean(value < alpha) - z_score*sqrt(mean(value < alpha)*(1-mean(value < alpha)))/length(value),
            upper_t1_bound = mean(value < alpha) + z_score*sqrt(mean(value < alpha)*(1-mean(value < alpha)))/length(value),
            t1_error = mean(value < alpha),
            ci = glue::glue("[{round(lower_t1_bound,4)}, {round(upper_t1_bound,4)}]")
            )






options(ordinalsimr.default_distributions = data.frame(c(0.2,0.2,0.3,0.2,0.1), c(0.4,0.3,0.2,0.05,0.05)) )

options(ordinalsimr.default_distributions = NULL )
