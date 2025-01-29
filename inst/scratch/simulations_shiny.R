library(rms)
###############
## 1. Bsp: iterations = 10000, n = 200, distribution ratio 50:50,
### group 1: 0.3, 0.3, 0.4; group 2: 0.5, 0.25, 0.25

wilcox <- numeric()
fisher <- numeric()
chisq_F <- numeric()
chisq_T <- numeric()
kruskal <- numeric()
lrm <- numeric()
indep <- numeric()

for (i in 1:10000) {
  set.seed(i)
  gr1 <- sample(x = 0:1, prob = c(0.3, 0.3, 0.4), size = 100, replace = TRUE)
  gr2 <- sample(x = 0:1, prob = c(0.5, 0.25, 0.25), size = 100, replace = TRUE)
  data <- data.frame(category = c(gr1, gr2), group = c(rep(0,100), rep(1,100)))


  wilcox[i] <- round(wilcox.test(gr1, gr2, paired = T, alternative = "two.sided")$p.val, 5)
  fisher[i] <- round(fisher.test(rbind(table(gr1),table(gr2)))$p.val, 5)
  chisq_F[i] <- round(chisq.test(rbind(table(gr1),table(gr2)), correct = FALSE)$p.val, 5)
  chisq_T[i] <- round(chisq.test(rbind(table(gr1),table(gr2)), correct = TRUE)$p.val, 5)
  kruskal[i] <- round(kruskal.test(c(gr1, gr2), c(rep(0,100), rep(1,100)))$p.val, 5)
  lrm[i] <- round(rms::lrm(category ~ group, data = data)$stats["P"], 5)
  indep[i] <- round(coin::pvalue(coin::independence_test(category ~ group, data = data, ytrafo = coin::rank_trafo)), 5)
}


mean(wilcox < 0.05)
# 0.8254; 0.8245
# in App: 0.83450

mean(fisher < 0.05)
# 0.7688; 0.7696
# in App: 0.76630

mean(chisq_F < 0.05)
# 0.7727; 0.7751
# in app: 0.76830

mean(chisq_T < 0.05)
# 0.7727; 0.7751
# in app: 0.76830

mean(kruskal < 0.05)
# 0.8428; 0.8397
# in App: 0.83480

mean(lrm < 0.05)
# 0.8451
# in App: 0.83770

mean(indep < 0.05)
# 0.8456
# in App: 0.83480

rx <- function(n) sample(x = c(1, 2, 3), prob = c(0.3, 0.3, 0.4), size = n, replace = TRUE)
ry <- function(n) sample(x = c(1, 2, 3), prob = c(0.5, 0.25, 0.25), size = n, replace = TRUE)

sim.power.wilcox.test(nx = 100, rx = rx, ny = 100, ry = ry)
# 0.8433
# in app:  0.83450




## 2. Bsp: iterations = 10000, n = 200, distribution ratio 50:50,
### group 1: 0.1, 0.1, 0.8; group 2: 0.5, 0.2, 0.3
wilcox <- numeric()
fisher <- numeric()
chisq_F <- numeric()
chisq_T <- numeric()
kruskal <- numeric()
lrm <- numeric()
indep <- numeric()

for (i in 1:10000) {
  gr1 <- sample(x = c(1, 2, 3), prob = c(0.1, 0.1, 0.8), size = 100, replace = TRUE)
  gr2 <- sample(x = c(1, 2, 3), prob = c(0.5, 0.2, 0.3), size = 100, replace = TRUE)
  data <- data.frame(category = c(gr1, gr2), group = c(rep(0,100), rep(1,100)))

  wilcox[i] <- round(wilcox.test(gr1, gr2, paired = T, alternative = "two.sided")$p.val, 5)
  fisher[i] <- round(fisher.test(rbind(table(gr1),table(gr2)))$p.val, 5)
  chisq_F[i] <- round(chisq.test(rbind(table(gr1),table(gr2)), correct = FALSE)$p.val, 5)
  chisq_T[i] <- round(chisq.test(rbind(table(gr1),table(gr2)), correct = TRUE)$p.val, 5)
  kruskal[i] <- round(kruskal.test(c(gr1, gr2), c(rep(0,100), rep(1,100)))$p.val, 5)
  lrm[i] <- round(lrm(category ~ group, data = data)$stats["P"], 5)
  indep[i] <- round(pvalue(independence_test(category ~ group, data = data, ytrafo = rank_trafo)), 5)
}



mean(wilcox < 0.05)
# 1
# in App: 1

mean(fisher < 0.05)
# 1
# in App: 1

mean(chisq_F < 0.05)
# 1
# in App: 1

mean(chisq_T < 0.05)
# 1
# in App: 1

mean(kruskal < 0.05)
# 1
# in App: 1

mean(lrm < 0.05)
# 1
# in App: 1

mean(indep < 0.05)
# 1
# in App: 1


rx <- function(n) sample(x = c(1, 2, 3), prob = c(0.1, 0.1, 0.8), size = n, replace = TRUE)
ry <- function(n) sample(x = c(1, 2, 3), prob = c(0.5, 0.2, 0.3), size = n, replace = TRUE)

sim.power.wilcox.test(nx = 100, rx = rx, ny = 100, ry = ry)
# 1
# in shiny: 1






## 3. Bsp: iterations = 10000, n = 200, distribution ratio 50:50,
### group 1: 0.3, 0.4, 0.3; group 2: 0.35, 0.35, 0.3
wilcox <- numeric()
fisher <- numeric()
chisq_F <- numeric()
chisq_T <- numeric()
kruskal <- numeric()
lrm <- numeric()
indep <- numeric()

for (i in 1:10000) {
  gr1 <- sample(x = c(1, 2, 3), prob = c(0.3, 0.4, 0.3), size = 100, replace = TRUE)
  gr2 <- sample(x = c(1, 2, 3), prob = c(0.35, 0.35, 0.3), size = 100, replace = TRUE)
  data <- data.frame(category = c(gr1, gr2), group = c(rep(0,100), rep(1,100)))

  wilcox[i] <- round(wilcox.test(gr1, gr2, paired = T, alternative = "two.sided")$p.val, 5)
  fisher[i] <- round(fisher.test(rbind(table(gr1),table(gr2)))$p.val, 5)
  chisq_F[i] <- round(chisq.test(rbind(table(gr1),table(gr2)), correct = FALSE)$p.val, 5)
  chisq_T[i] <- round(chisq.test(rbind(table(gr1),table(gr2)), correct = TRUE)$p.val, 5)
  kruskal[i] <- round(kruskal.test(c(gr1, gr2), c(rep(0,100), rep(1,100)))$p.val, 5)
  lrm[i] <- round(lrm(category ~ group, data = data)$stats["P"], 5)
  indep[i] <- round(pvalue(independence_test(category ~ group, data = data, ytrafo = rank_trafo)), 5)
}



mean(wilcox < 0.05)
# 0.0744
# in App: 0.07520

mean(fisher < 0.05)
# 0.1093
# in App: 0.10720

mean(chisq_F < 0.05)
# 0.1125
# in App: 0.10910

mean(chisq_T < 0.05)
# 0.1125
# in App: 0.10910

mean(kruskal < 0.05)
# 0.0781
# in App: 0.07550

mean(lrm < 0.05)
# 0.0736
# in App: 0.07670

mean(indep < 0.05)
# 0.0743
# in App:	0.07550

rx <- function(n) sample(x = c(1, 2, 3), prob = c(0.3, 0.4, 0.3), size = n, replace = TRUE)
ry <- function(n) sample(x = c(1, 2, 3), prob = c(0.35, 0.35, 0.3), size = n, replace = TRUE)

sim.power.wilcox.test(nx = 100, rx = rx, ny = 100, ry = ry, iter = 1000)
# 0.068
# in shiny: 0.068


## 4. Bsp: iterations = 10000, n = 300, distribution ratio 50:50,
### group 1: 0.0, 0.2, 0.8; group 2: 0.1, 0.3, 0.6
wilcox <- numeric()
fisher <- numeric()
chisq_F <- numeric()
chisq_T <- numeric()
kruskal <- numeric()
lrm <- numeric()
indep <- numeric()

for (i in 1:10000) {
  gr1 <- sample(x = c(1, 2, 3), prob = c(0.0, 0.2, 0.8), size = 150, replace = TRUE)
  gr2 <- sample(x = c(1, 2, 3), prob = c(0.1, 0.3, 0.6), size = 150, replace = TRUE)
  data <- data.frame(category = c(gr1, gr2), group = c(rep(0,150), rep(1,150)))

  wilcox[i] <- round(wilcox.test(gr1, gr2, paired = T, alternative = "two.sided")$p.val, 5)
  fisher[i] <- round(fisher.test(rbind(table(gr1),table(gr2)))$p.val, 5)
  chisq_F[i] <- round(chisq.test(rbind(table(gr1),table(gr2)), correct = FALSE)$p.val, 5)
  chisq_T[i] <- round(chisq.test(rbind(table(gr1),table(gr2)), correct = TRUE)$p.val, 5)
  kruskal[i] <- round(kruskal.test(c(gr1, gr2), c(rep(0,150), rep(1,150)))$p.val, 5)
  lrm[i] <- round(lrm(category ~ group, data = data)$stats["P"], 5)
  indep[i] <- round(pvalue(independence_test(category ~ group, data = data, ytrafo = rank_trafo)), 5)
}


mean(wilcox < 0.05)
# 0.9987
# in App: 0.98760

mean(fisher < 0.05)
# 1
# in App: 0.99990

mean(chisq_F < 0.05)
# 1
# in App:0.99990

mean(chisq_T < 0.05)
# 1
# in App: 0.99990

mean(kruskal < 0.05)
#  0.9878
# in App: 0.98760

mean(lrm < 0.05)
# 0.988
# in App: 0.98790

mean(indep < 0.05)
# 0.987
# in App: 0.98760
