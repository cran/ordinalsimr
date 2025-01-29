



rng_info <- list(rng_kind = c("Mersenne-Twister", "Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
        "Knuth-TAOCP-2002", "Knuth-TAOCP", "L'Ecuyer-CMRG"),
     rng_normal_kind = c("Inversion","Kinderman-Ramage", "Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller"),
     rng_sample_kind = c("Rejection", "Rounding")
     ) %>%
  expand.grid() %>%
  as.data.frame() %>%
  mutate(across(everything(), as.character))




purrr::pmap(rng_info, function(rng_kind, rng_normal_kind, rng_sample_kind){

  run_simulations(
    sample_size = 30,
    sample_prob = c(0.5,0.5),
    prob0 = c(0.25,0.25,0.25,0.25),
    prob1 = c(0.4,0.3,0.2,0.1),
    niter = 2,
    .rng_kind = rng_kind,
    .rng_normal_kind = rng_normal_kind,
    .rng_sample_kind = rng_sample_kind
  )
})




