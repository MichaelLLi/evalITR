# Internal implementation shared by GATE, GATEcv, and the GATES tests.
# A is the analytic covariance of one validation fold; B is the covariance
# across scoring rules evaluated on the SAME full sample. The analytic
# cross-fit covariance is sum(A_l) / L^2 + B, under score stability.

.gate_validate <- function(T, tau, Y, ind = NULL, ngates = 5,
                           centered = FALSE) {
  if ((!is.numeric(T) && !is.logical(T)) || anyNA(T) ||
      any(!T %in% c(0, 1))) stop("Treatment should be binary.")
  if (!is.numeric(Y) || any(!is.finite(Y)))
    stop("Y should contain finite numeric outcomes.")
  if (length(Y) == 0L) stop("The data should have positive length.")
  if (length(T) != length(Y)) stop("All the data should have the same length.")
  if (length(ngates) != 1L || !is.numeric(ngates) || !is.finite(ngates) ||
      ngates < 2 || ngates != floor(ngates))
    stop("ngates should be an integer of at least 2.")
  if (!is.logical(centered) || length(centered) != 1L || is.na(centered))
    stop("centered should be TRUE or FALSE.")
  if (!is.numeric(tau) || any(!is.finite(tau)))
    stop("tau should contain finite numeric scores.")
  if (is.null(ind)) {
    # Prediction methods such as grf can return an n-by-1 matrix.
    if (is.matrix(tau) && ncol(tau) == 1L) tau <- as.numeric(tau)
    if (!is.null(dim(tau)) || length(tau) != length(Y))
      stop("tau should be a score vector with the same length as Y.")
    tau <- matrix(tau, ncol = 1L)
    ind <- rep.int(1L, length(Y))
  } else {
    if (!is.numeric(ind) || length(ind) != length(Y) || anyNA(ind) ||
        any(!is.finite(ind)) || any(ind < 1 | ind != floor(ind)))
      stop("ind should contain consecutive integer fold labels starting at 1.")
    L <- max(ind)
    if (L < 2L || L > length(Y) ||
        !identical(sort(unique(as.integer(ind))), seq_len(L)))
      stop("ind should identify at least two nonempty consecutive folds.")
    if (!is.matrix(tau) || nrow(tau) != length(Y) || ncol(tau) != L)
      stop("tau should have one row per outcome and one column per fold.")
  }
  for (l in seq_len(ncol(tau))) {
    arm <- T[ind == l]
    if (length(arm) < ngates)
      stop("Each validation fold should contain at least ngates observations.")
    if (sum(arm == 1) < 2L || sum(arm == 0) < 2L)
      stop("Each validation fold needs at least two observations in each treatment arm.")
  }
  list(T = as.numeric(T), tau = tau, Y = Y, ind = ind,
       K = as.integer(ngates), L = ncol(tau))
}

# One random priority per observation is reused across all scoring rules.
# This prevents row-order ties from depending on treatment and prevents
# identical tied scores from creating spurious between-rule variation.
.gate_priority <- function(tau) {
  if (any(vapply(seq_len(ncol(tau)), function(l)
    anyDuplicated(tau[, l]) > 0L, logical(1)))) {
    warning(paste("Tied scores: ties are broken randomly, independently of treatment.",
                  "Use set.seed() for reproducibility."), call. = FALSE)
    return(stats::runif(nrow(tau)))
  }
  seq_len(nrow(tau))
}

.gate_partition <- function(score, priority, K) {
  # Match ntile for distinct scores, including its larger-first buckets.
  ord <- order(score, priority)
  sizes <- rep.int(length(score) %/% K, K)
  extra <- length(score) %% K
  if (extra > 0L) sizes[seq_len(extra)] <- sizes[seq_len(extra)] + 1L
  label <- integer(length(score))
  label[ord] <- rep.int(seq_len(K), sizes)
  groups <- outer(label, seq_len(K), `==`) * 1
  ranks <- integer(length(score))
  ranks[ord] <- seq_along(ord)
  attr(groups, "rank") <- ranks
  groups
}

.gate_psd <- function(V) {
  V <- (V + t(V)) / 2
  eig <- eigen(V, symmetric = TRUE)
  if (min(eig$values) >= 0) return(V)
  # Remove numerical negative eigenvalues from A alone, leaving the
  # positive semidefinite training component B intact.
  tcrossprod(sweep(eig$vectors, 2L, sqrt(pmax(eig$values, 0)), `*`))
}

.gate_moments <- function(T, Y, groups, deviations = FALSE) {
  m <- length(Y)
  K <- ncol(groups)
  m1 <- sum(T)
  m0 <- m - m1
  p <- colMeans(groups)
  Z <- K * groups * Y
  estimate <- colSums(Z[T == 1, , drop = FALSE]) / m1 -
    colSums(Z[T == 0, , drop = FALSE]) / m0
  # Quantile influence correction. attr(groups, "rank") contains the full
  # ordering, including the shared random tie priorities.
  ranks <- attr(groups, "rank")
  boundary <- matrix(0, m, K)
  cumulative <- cumsum(p)
  # Local differences in arm means consistently estimate the CATE at each
  # interior quantile. The neighborhood grows but its fraction tends to zero.
  neighbors <- max(2L, ceiling(m^0.7 / 2))
  for (j in seq_len(K - 1L)) {
    cutoff <- sum(groups[, seq_len(j), drop = FALSE]) + 0.5
    near <- order(abs(ranks - cutoff))
    treated <- near[T[near] == 1][seq_len(min(neighbors, m1))]
    control <- near[T[near] == 0][seq_len(min(neighbors, m0))]
    effect <- mean(Y[treated]) - mean(Y[control])
    qif <- effect * (cumulative[j] - as.numeric(ranks < cutoff))
    boundary[, j] <- boundary[, j] + K * qif
    boundary[, j + 1L] <- boundary[, j + 1L] - K * qif
  }
  treated_values <- Z + m1 / m * boundary
  control_values <- -Z + m0 / m * boundary
  V <- stats::cov(treated_values[T == 1, , drop = FALSE]) / m1 +
    stats::cov(control_values[T == 0, , drop = FALSE]) / m0
  if (deviations) {
    P <- diag(K) - outer(p, rep.int(1, K))
    estimate <- as.numeric(P %*% estimate)
    V <- P %*% V %*% t(P)
  }
  if (any(colSums(groups[T == 1, , drop = FALSE]) == 0) ||
      any(colSums(groups[T == 0, , drop = FALSE]) == 0)) V[,] <- NA_real_
  list(estimate = as.numeric(estimate), covariance = V)
}

.gate_fit <- function(T, tau, Y, ind = NULL, ngates = 5,
                      centered = FALSE, deviations = FALSE) {
  dat <- .gate_validate(T, tau, Y, ind, ngates, centered)
  T <- dat$T; tau <- dat$tau; ind <- dat$ind
  K <- dat$K; L <- dat$L
  if (centered) Y <- Y - mean(Y)
  priority <- .gate_priority(tau)
  estimates <- matrix(NA_real_, L, K)
  full <- matrix(NA_real_, L, K)
  A <- matrix(0, K, K)
  for (l in seq_len(L)) {
    idx <- ind == l
    groups <- .gate_partition(tau[idx, l], priority[idx], K)
    moments <- .gate_moments(T[idx], Y[idx], groups, deviations)
    estimates[l, ] <- moments$estimate
    A <- A + moments$covariance / L
    if (L > 1L) {
      # Re-estimate quantiles on this common sample. Validation-fold cutoffs
      # would add quantile noise to B even for identical scoring rules.
      groups_full <- .gate_partition(tau[, l], priority, K)
      full_estimate <- K * (colSums((groups_full * Y)[T == 1, , drop = FALSE]) / sum(T) -
        colSums((groups_full * Y)[T == 0, , drop = FALSE]) / sum(1 - T))
      if (deviations) full_estimate <- full_estimate - colMeans(groups_full) * sum(full_estimate)
      full[l, ] <- full_estimate
    }
  }
  B <- if (L > 1L) stats::cov(full) else matrix(0, K, K)
  if (any(!is.finite(A))) {
    warning(paste("At least one validation group has an empty treatment arm;",
                  "its covariance cannot be estimated. Use fewer groups or larger folds."),
            call. = FALSE)
    V <- matrix(NA_real_, K, K)
  } else {
    A <- .gate_psd(A)
    V <- A / L + B
  }
  list(estimate = colMeans(estimates), covariance = V,
       within = A, training = B, fold_estimates = estimates)
}

.gate_contrasts <- function(fit) {
  K <- length(fit$estimate)
  C <- diff(diag(K))
  list(estimate = as.numeric(C %*% fit$estimate),
       covariance = C %*% fit$covariance %*% t(C))
}

.gate_wald <- function(fit) {
  z <- .gate_contrasts(fit)
  if (any(!is.finite(z$covariance))) return(list(stat = NA_real_, pval = NA_real_))
  eig <- eigen(z$covariance, symmetric = TRUE)
  tol <- max(abs(eig$values)) * 1e-10
  keep <- eig$values > tol
  coordinates <- as.numeric(crossprod(eig$vectors, z$estimate))
  if (!all(keep) && any(abs(coordinates[!keep]) >
                        1e-8 * max(1, abs(z$estimate))))
    return(list(stat = NA_real_, pval = NA_real_))
  if (!any(keep)) return(list(stat = 0, pval = 1))
  statistic <- sum(coordinates[keep]^2 / eig$values[keep])
  list(stat = statistic,
       pval = stats::pchisq(statistic, df = sum(keep), lower.tail = FALSE))
}

.gate_order_test <- function(fit, nsim) {
  if (length(nsim) != 1L || !is.numeric(nsim) || !is.finite(nsim) ||
      nsim < 2 || nsim != floor(nsim)) stop("nsim should be an integer of at least 2.")
  z <- .gate_contrasts(fit)
  if (any(!is.finite(z$covariance))) return(list(stat = NA_real_, pval = NA_real_))
  eig <- eigen(z$covariance, symmetric = TRUE, only.values = TRUE)$values
  if (max(abs(eig)) == 0 && all(z$estimate >= 0))
    return(list(stat = 0, pval = 1))
  if (min(eig) <= max(abs(eig)) * 1e-10)
    return(list(stat = NA_real_, pval = NA_real_))
  precision <- solve(z$covariance)
  distance <- function(x) {
    optimum <- quadprog::solve.QP(precision, as.numeric(precision %*% x),
                                  diag(length(x)))$solution
    residual <- x - optimum
    as.numeric(crossprod(residual, precision %*% residual))
  }
  statistic <- distance(z$estimate)
  if (statistic < 1e-12) return(list(stat = 0, pval = 1))
  draws <- MASS::mvrnorm(nsim, rep.int(0, length(z$estimate)), z$covariance)
  draws <- matrix(draws, nrow = nsim)
  simulated <- apply(draws, 1L, distance)
  list(stat = statistic, pval = (1 + sum(simulated >= statistic)) / (nsim + 1))
}
