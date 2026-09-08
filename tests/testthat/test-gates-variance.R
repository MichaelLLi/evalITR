gate_fixture <- function() {
  set.seed(812)
  n <- 600L
  x <- stats::runif(n)
  ind <- rep(1:3, each = 200L)
  T <- rep(c(0, 1), n / 2)
  Y <- stats::rnorm(n) + 0.2 * T * x
  list(T = T, Y = Y, tau = matrix(x, n, 3), ind = ind)
}

test_that("variance reduces to independent-fold averaging for a fixed rule", {
  d <- gate_fixture()
  fit <- .gate_fit(d$T, d$tau, d$Y, d$ind, 4)
  expected <- vapply(1:3, function(l) {
    ix <- d$ind == l
    diag(.gate_fit(d$T[ix], d$tau[ix, l], d$Y[ix], ngates = 4)$covariance)
  }, numeric(4))
  expect_equal(fit$training, matrix(0, 4, 4), tolerance = 1e-14)
  expect_equal(diag(fit$covariance), rowMeans(expected) / 3, tolerance = 1e-12)
  out <- GATEcv(d$T, d$tau, d$Y, d$ind, 4)
  expect_equal(out$sd^2, diag(fit$covariance))
  fold_estimates <- vapply(1:3, function(l) {
    ix <- d$ind == l
    group <- dplyr::ntile(d$tau[ix, l], 4)
    vapply(1:4, function(k)
      4 * (mean(d$Y[ix][d$T[ix] == 1] * (group[d$T[ix] == 1] == k)) -
           mean(d$Y[ix][d$T[ix] == 0] * (group[d$T[ix] == 0] == k))), numeric(1))
  }, numeric(4))
  expect_equal(out$gate, rowMeans(fold_estimates))
})

test_that("quantile uncertainty matches a closed-form low-noise benchmark", {
  # X uniform(0,1), Y(1)=X, Y(0)=0, treatment probability 1/2.
  # The lower-half GATE has asymptotic variance 5/(24*n).
  # Omitting quantile uncertainty gives only 7/(48*n).
  n <- 10000L
  x <- (seq_len(n) - 0.5) / n
  T <- rep(c(0, 1), n / 2)
  fit <- .gate_fit(T, x, T * x, ngates = 2)
  expect_equal(n * fit$covariance[1, 1], 5/24, tolerance = 0.001)
})

test_that("fixed-score inference accepts one-column prediction matrices", {
  d <- gate_fixture()
  expect_equal(GATE(d$T, d$tau[, 1, drop = FALSE], d$Y),
               GATE(d$T, d$tau[, 1], d$Y))
  expect_equal(het.test(d$T, d$tau[, 1, drop = FALSE], d$Y),
               het.test(d$T, d$tau[, 1], d$Y))
})

test_that("training variation uses common-sample ranks and is not divided by L", {
  d <- gate_fixture()
  d$tau[, 2] <- 100 + 3 * d$tau[, 2]
  d$tau[, 3] <- exp(d$tau[, 3])
  expect_equal(.gate_fit(d$T, d$tau, d$Y, d$ind, 3)$training,
               matrix(0, 3, 3))
  d$tau[, 2] <- -d$tau[, 2]
  fit <- .gate_fit(d$T, d$tau, d$Y, d$ind, 3)
  full <- t(vapply(1:3, function(l)
    GATE(d$T, d$tau[, l], d$Y, 3)$gate, numeric(3)))
  expect_equal(fit$training, stats::cov(full), tolerance = 1e-12)
  expect_gt(sum(diag(fit$training)), 0)
  expect_equal(fit$covariance, fit$within / 3 + stats::cov(full))
})

test_that("fold relabeling preserves results with unequal arm counts and fold sizes", {
  d <- gate_fixture()
  d$ind[200] <- 2
  d$T[1:16] <- 1
  perm <- c(3, 1, 2)
  for (fun in list(GATEcv, hetcv.test)) {
    a <- fun(d$T, d$tau, d$Y, d$ind, 3)
    b <- fun(d$T, d$tau[, perm], d$Y, match(d$ind, perm), 3)
    expect_equal(a, b, tolerance = 1e-12)
  }
  set.seed(83)
  a <- consistcv.test(d$T, d$tau, d$Y, d$ind, 3, nsim = 199)
  set.seed(83)
  b <- consistcv.test(d$T, d$tau[, perm], d$Y, match(d$ind, perm), 3, nsim = 199)
  expect_equal(a, b, tolerance = 1e-10)
})

test_that("deviation covariance is the linear transform of the GATE covariance", {
  d <- gate_fixture()
  raw <- .gate_fit(d$T, d$tau, d$Y, d$ind, 4)
  dev <- .gate_fit(d$T, d$tau, d$Y, d$ind, 4, deviations = TRUE)
  P <- diag(4) - matrix(1/4, 4, 4)
  expect_equal(dev$estimate, as.numeric(P %*% raw$estimate), tolerance = 1e-12)
  expect_equal(dev$covariance, P %*% raw$covariance %*% P, tolerance = 1e-12)
  expect_equal(rowSums(dev$covariance), rep(0, 4), tolerance = 1e-12)
  expect_gte(min(eigen(dev$covariance, symmetric = TRUE)$values), -1e-12)
})

test_that("Wald inference uses the rank of the contrast covariance", {
  P <- diag(3) - matrix(1/3, 3, 3)
  fit <- list(estimate = c(-1, 0, 1), covariance = P)
  out <- .gate_wald(fit)
  expect_equal(out$stat, 2)
  expect_equal(out$pval, stats::pchisq(2, 2, lower.tail = FALSE))
  fit$covariance[,] <- 0
  expect_true(is.na(.gate_wald(fit)$pval))
  fit$estimate[] <- 0
  expect_equal(.gate_wald(fit), list(stat = 0, pval = 1))
})

test_that("order inference agrees with the two-group Gaussian benchmark", {
  set.seed(921)
  out <- .gate_order_test(list(estimate = c(1, 0), covariance = diag(2)), 3999)
  expect_equal(out$stat, 0.5, tolerance = 1e-12)
  expect_lt(abs(out$pval - stats::pnorm(-1 / sqrt(2))), 0.025)
  expect_equal(.gate_order_test(list(estimate = c(0, 1), covariance = diag(2)), 99),
               list(stat = 0, pval = 1))
})

test_that("centered tests are invariant to the outcome origin", {
  d <- gate_fixture()
  expect_equal(hetcv.test(d$T, d$tau, d$Y, d$ind, 3),
               hetcv.test(d$T, d$tau, d$Y + 10000, d$ind, 3), tolerance = 1e-9)
  expect_equal(het.test(d$T, d$tau[, 1], d$Y, 3),
               het.test(d$T, d$tau[, 1], d$Y + 10000, 3), tolerance = 1e-9)
})

test_that("tied scores have reproducible shared tie breaking", {
  d <- gate_fixture()
  d$tau[,] <- 1
  set.seed(22)
  expect_warning(a <- .gate_fit(d$T, d$tau, d$Y, d$ind, 3), "Tied scores")
  set.seed(22)
  expect_warning(b <- .gate_fit(d$T, d$tau, d$Y, d$ind, 3), "Tied scores")
  expect_identical(a, b)
  expect_equal(a$training, matrix(0, 3, 3))
  d <- gate_fixture()
  seed <- .Random.seed
  GATEcv(d$T, d$tau, d$Y, d$ind, 3)
  expect_identical(.Random.seed, seed)
})

test_that("invalid designs fail clearly and sparse groups have unavailable inference", {
  d <- gate_fixture()
  expect_error(GATEcv(d$T, d$tau[, 1], d$Y, d$ind), "one row")
  expect_error(GATEcv(d$T, d$tau, d$Y, rep(1, 600)), "at least two")
  expect_error(GATEcv(d$T, d$tau, d$Y, d$ind + 1), "consecutive")
  expect_error(GATEcv(d$T, d$tau, d$Y, d$ind, 1), "at least 2")
  expect_error(GATEcv(d$T, d$tau, d$Y, d$ind, centered = NA), "centered")
  expect_error(GATEcv(rep(1, 600), d$tau, d$Y, d$ind), "each treatment arm")
  d$Y[1] <- NA_real_
  expect_error(GATEcv(d$T, d$tau, d$Y, d$ind), "finite numeric outcomes")
  expect_warning(out <- GATEcv(rep(c(0, 0, 1, 1), 2), matrix(1:8, 8, 2),
                               1:8, rep(1:2, each = 4), 2), "empty treatment arm")
  expect_true(all(is.na(out$sd)))
  expect_true(all(is.finite(out$gate)))
  expect_error(.gate_order_test(list(), 0), "nsim")
})

test_that("high-level tests use each algorithm's full prediction matrix", {
  d <- gate_fixture()
  folds <- lapply(1:3, function(l) list(tau_cv = d$tau[, l]))
  reversed <- lapply(1:3, function(l) list(tau_cv = -d$tau[, l]))
  fit <- list(estimates = list(
    params = list(cv = TRUE, n_folds = 3, ngates = 3),
    fit_ml = list(first = folds, second = reversed),
    Tcv = d$T, Ycv = d$Y, indcv = d$ind),
    df = list(algorithms = c("first", "second"), outcome = "Y"))
  out <- test_itr(fit, nsim = 19)
  expect_equal(out$hetcv$first, hetcv.test(d$T, d$tau, d$Y, d$ind, 3))
  expect_equal(out$hetcv$second, hetcv.test(d$T, -d$tau, d$Y, d$ind, 3))
  for (result in out$consistcv)
    expect_equal(result$pval * 20, round(result$pval * 20))
})
