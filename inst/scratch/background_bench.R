
devtools::load_all()
library(furrr)
plan(multisession, workers = 4)
print("hello")

microbenchmark::microbenchmark(
  testing_fxn(sample_size = c(40, 80, 120),
              sample_prob = c(0.5,0.5),
              prob0 = c(0.1,0.2,0.3,0.4),
              prob1 = c(0.6,0.2,0.1,0.1),
              niter = 100
  ),

  map(c(40, 80, 120),
      ~run_simulations(sample_size = .x,
                       sample_prob = c(0.5,0.5),
                       prob0 = c(0.1,0.2,0.3,0.4),
                       prob1 = c(0.6,0.2,0.1,0.1),
                       niter = 100
      )[[1]]
  ),

  times = 5
) %>%
  print()
