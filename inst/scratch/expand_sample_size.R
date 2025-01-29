


testing_fxn <- function(sample_size, sample_prob, prob0, prob1, niter) {

  # Check equal vector lengths
  assert_that( length(prob0) == length(prob1) )
  # Check probabilities for both groups sum to 1
  assert_that( dplyr::near(sum(prob0), 1), msg = "Probability for Group 1 does not sum to 1." )
  assert_that( dplyr::near(sum(prob1), 1), msg = "Probability for Group 2 does not sum to 1" )


  K <- length(prob0)
  p_values <- matrix(NA,niter,7)
  colnames(p_values) <- c("Wilcoxon", "Fisher", "Chi Squared\n(No Correction)", "Chi Squared\n(Correction)",
                          "Prop. Odds", "Kruskal-Wallis", "Coin Indep. Test")


  purrr::map(sample_size,
             ~{
               sample_size_nested <- .x
               initial_groups <- purrr::map(1:niter, ~assign_groups(sample_size = sample_size_nested,
                                                  sample_prob = sample_prob,
                                                  prob0 = prob0, prob1 = prob1,
                                                  seed = .x)   )

               p_values <- initial_groups %>%
                 sapply(.,function(x) ordinal_tests(x[["x"]], x[["y"]])) %>%
                 t()

               initial_groups_formatted <- initial_groups %>%
                 map_df(~tibble(y = list(.x[["y"]]), x = list(.x[["x"]]),
                                n_null = .x[["n_null"]], n_intervene = .x[["n_intervene"]],
                                sample_size = .x[["sample_size"]], K = .x[["K"]])
                        ) %>%
                 mutate(run = row_number(), .before = y)

               return( bind_cols(initial_groups_formatted, p_values) )
             },

             .progress = list(caller = environment(),
                              format = "Running {niter} iterations on {length(sample_size)} sample sizes. Progress: {cli::pb_bar} {cli::pb_percent} {cli::pb_eta}")

             ) %>%
    purrr::set_names(glue::glue("sample_size_{sample_size}"))

}




############################
testing_fxn(sample_size = c(40:45),
                sample_prob = c(0.5,0.5),
                prob0 = c(0.1,0.2,0.3,0.4),
                prob1 = c(0.6,0.2,0.1,0.1),
                niter = 100
            )


debug(testing_fxn)


##########################
library(furrr)
plan(multisession, workers = 2)

system.time({
microbenchmark::microbenchmark(
  testing_fxn(sample_size = c(40, 80, 120),
              sample_prob = c(0.5,0.5),
              prob0 = c(0.1,0.2,0.3,0.4),
              prob1 = c(0.6,0.2,0.1,0.1),
              niter = 40
  ),

  map(c(40, 80, 120),
    ~run_simulations(sample_size = .x,
                    sample_prob = c(0.5,0.5),
                    prob0 = c(0.1,0.2,0.3,0.4),
                    prob1 = c(0.6,0.2,0.1,0.1),
                    niter = 40
    )[[1]]
  ),

  times = 10
)

})

