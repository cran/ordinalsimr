


dqrng::register_methods()

dqrng::dqset.seed(5)
# set.seed(5)

  sample_size = 30
  prob0 <- c(.4, .3, .3)
  prob1 <- c(.9, .05, .05)


  y <- factor(dqrng::dqsample(x = 0:1, size = sample_size, replace = TRUE, prob = c(.5,.5)))
  n_null <- sum(y == 0)
  n_intervene <- sample_size - n_null
  x <- rep(1, sample_size)
  K <- length(prob0)

  x[y == 0] <- dqrng::dqsample(1:K, n_null, replace = TRUE, prob = prob0)
  x[y == 1] <- dqrng::dqsample(1:K, n_intervene, replace = TRUE, prob = prob1)

  x

  unique(x[y==0])
  unique(x[y==1])

  length(unique(x[y==0])) < K
  length(unique(x[y==1])) < K





ordinalsimr::ordinal_tests(x, y)
