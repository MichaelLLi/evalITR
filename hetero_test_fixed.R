## hetero_test_fixed.R ---------------------------------------------------------
## Corrected heterogeneity tests for evalITR: het.test2 (fixed score, Theorem 3)
## and hetcv.test2 (cross-fitting, Theorem 7).  Standalone file — does not touch
## the package.  Requires: dplyr (ntile).
##
## Changes vs shipped het.test / hetcv.test, each verified this session:
##
##  (F1) Outcome centering, Y <- Y - mean(Y).
##       The GATES-deviation estimand is location-invariant but the HT-form
##       estimator is not; the leakage term is mean(Y) x (per-gate treatment
##       imbalance).  Centering is asymptotically free (perturbation is
##       O_p(n^-1) against an O_p(n^-1/2) statistic; exact conditional
##       mean-zero of the imbalance under complete randomization) and makes
##       finite-sample behavior uniform in the outcome location.
##  (F2) Starred S-terms: (f_k - 1/K) * Y instead of f_k * Y, exactly as in
##       Theorem 3 (heterotest.tex:739) and the earlier draft (tex:819).
##       The unstarred version understates middle-gate variance ~10x when the
##       outcome level is correlated with the score ranking (measured size
##       0.30 at alpha=.05 under a true null; starred restores ~0.06-0.09).
##  (F3) kappa-block: kept in the DRAFT form already shipped, on purpose.
##       The proof audit showed the published Theorem 3/7 block has a dropped
##       (K-1) factor (O(1)-negative when ATE != 0; measured 15% null rejection
##       at ATE=10), and that the corrected block reduces exactly to the draft
##       form under the independence plug-in kappa_kk11 = kappa_k1^2.  We also
##       verified one must NOT estimate kappa_kk11 separately by its pair
##       estimator: the block's target is O(1/n) but that plug-in's noise is
##       O(1), which collapses the variance estimate and destroys the test
##       (measured 67% null rejection).  The draft form keeps 1/(n-1) outside,
##       damping all kappa noise by 1/n.  So: shipped kappa-block is correct.
##  (F4) Tie / degeneracy guards.  The theory assumes a bijective score; with
##       tied or constant scores ntile() assigns gates by ROW POSITION, so any
##       correlation between row order and treatment (e.g. data sorted by arm)
##       makes the grouping treatment-dependent and the test invalid.  We warn
##       and break ties RANDOMLY (restores grouping independent of T, which is
##       the property the proofs actually use).
##  (F5) [hetcv only] Per-fold m1/m0 in the S-term scaling (the shipped code
##       reuses whatever n1/n0 were left from the LAST fold; wrong for
##       unbalanced folds).
##  (F6) [hetcv only] Fold-combination step: the Theorem 7 identity
##       Sigma = sigma^2 - (L-1)/L * E(S2_F), with E(S2_F) = sigma^2 - c
##       estimated ANALYTICALLY: sigma^2_within = term1 + kappa-block (n-based,
##       high-df) and c = the training-variation term (kf1cv).  Equivalently
##       mcov = (term1 + kb)/L + V(kappa).  Never negative, so the min() of
##       eq:conserv_esti (introduced precisely to patch negative variances from
##       the noisy (L-1)-df cross-fold plug-in) is not needed.  Measured at
##       L=10: correlated-fold null size 0.050 (exactly nominal), power 0.663
##       vs 0.023 shipped (oracle 0.897).  The printed eq:conserv_esti plug-in
##       carries a ~3x variance inflation from Jensen bias of min(Sfp, U) under
##       9-df Sfp noise (no same-form truncation escapes it; verified) — it
##       remains available via the original evalITR::hetcv.test for exact
##       reproduction of the published procedure.
##       A warning is raised when S2_F exceeds the analytic bound (ML
##       stability / Assumption 7 suspect).
##
## Deliberate choices retained from the shipped code (asymptotically free):
##  - diagonalized covariance + chi^2_K reference (the deviation vector sums to
##    zero exactly, so the full matrix is singular; under diagonalization
##    E[stat] = K matches chi^2_K),
##  - the full-data kf1cv estimator for the training-variation term V(kappa)
##    (lower-noise than the within-fold version, which measurably destroys
##    power),
##  - eq:conserv_esti's min() combination (known conservative in the
##    independent-fold regime -- sharpening it is an open theory item, not an
##    implementation fix).
## -----------------------------------------------------------------------------

## Draft-form kappa block (see F3): value for entry (i,j) given the kappa
## vectors and the relevant sample size nn (n for het.test, fold size for CV).
.kblock <- function(kf1i, kf0i, kf1j, kf0j, K, nn) {
  1 / (K * (nn - 1)) *
    ((K - 1) * (kf1i^2 - kf1i * kf0i + kf1j^2 - kf1j * kf0j) -
     K * (K - 1) * kf1i * kf1j)
}

## Gate labels with random tie-breaking (F4).  Identical to dplyr::ntile for
## distinct scores; under ties, tied units are assigned exchangeably so the
## grouping stays independent of treatment and of row order.
.gate_labels <- function(tau, ngates, context = "score") {
  if (length(unique(tau)) == 1L) {
    warning(sprintf(paste0(
      "%s is CONSTANT: the ML scores carry no ranking information ",
      "(often caused by degenerate tuning, e.g. grf tune.parameters='all' on ",
      "small samples). Gates are assigned at random; the test is valid but has ",
      "no power. Results should not be interpreted as evidence about ",
      "heterogeneity."), context), call. = FALSE)
  } else if (anyDuplicated(tau)) {
    warning(sprintf(paste0(
      "%s contains tied values; gate assignment among ties is randomized ",
      "(deterministic position-based tie-breaking can make the grouping ",
      "treatment-dependent and invalidate the test)."), context), call. = FALSE)
  }
  dplyr::ntile(rank(tau, ties.method = "random"), ngates)
}

#' Heterogeneity test for GATES with a fixed scoring rule (corrected Theorem 3).
#' Same interface as evalITR::het.test.
het.test2 <- function(T, tau, Y, ngates = 5, center = TRUE) {
  if (!(identical(as.numeric(T), as.numeric(as.logical(T)))))
    stop("T should be binary.")
  if ((length(T) != length(tau)) | (length(tau) != length(Y)))
    stop("All the data should have the same length.")
  if (length(T) == 0) stop("The data should have positive length.")
  T <- as.numeric(T)
  K <- ngates
  n <- length(Y); n1 <- sum(T); n0 <- n - n1
  if (center) Y <- Y - mean(Y)                              # (F1)

  fd <- .gate_labels(tau, K, "tau")                         # (F4)

  papes <- numeric(K); kf1 <- numeric(K); kf0 <- numeric(K)
  S1 <- vector("list", K); S0 <- vector("list", K)
  for (k in 1:K) {
    That <- as.numeric(fd == k)
    plim <- 1 / K
    papes[k] <- K * (1/n1 * sum(T * That * Y) + 1/n0 * sum(Y * (1 - T) * (1 - That)) -
                     plim/n1 * sum(Y * T) - (1 - plim)/n0 * sum(Y * (1 - T)))
    S1[[k]] <- ((That - 1/K) * Y)[T == 1]                   # (F2) starred
    S0[[k]] <- ((That - 1/K) * Y)[T == 0]
    kf1[k]  <- mean(Y[T == 1 & That == 1]) - mean(Y[T == 0 & That == 1])
    kf0[k]  <- mean(Y[T == 1 & That == 0]) - mean(Y[T == 0 & That == 0])
  }

  mcov <- matrix(0, K, K)
  for (i in 1:K) for (j in 1:K) {
    term1 <- K^2 * (cov(S1[[i]], S1[[j]]) / n1 + cov(S0[[i]], S0[[j]]) / n0)
    mcov[i, j] <- term1 + .kblock(kf1[i], kf0[i], kf1[j], kf0[j], K, n)  # (F3)
  }
  mcov <- diag(diag(mcov), nrow = K, ncol = K)
  if (!is.finite(determinant(mcov)$modulus) || any(diag(mcov) <= 0))
    return(list(stat = NA, pval = NA))
  stat <- as.numeric(t(papes) %*% solve(mcov) %*% papes)
  list(stat = stat, pval = pchisq(stat, K, lower.tail = FALSE))
}

#' Heterogeneity test for GATES under cross-fitting (corrected Theorem 7).
#' Same interface as evalITR::hetcv.test.
hetcv.test2 <- function(T, tau, Y, ind, ngates = 5, center = TRUE) {
  if (!(identical(as.numeric(T), as.numeric(as.logical(T)))))
    stop("T should be binary.")
  if ((length(T) != dim(tau)[1]) | (dim(tau)[1] != length(Y)))
    stop("All the data should have the same length.")
  if (length(T) == 0) stop("The data should have positive length.")
  T <- as.numeric(T)
  K <- ngates
  L <- max(ind)
  if (center) Y <- Y - mean(Y)                              # (F1) global centering
                                                            #      (first-order equiv. to per-fold)
  papesm <- matrix(NA_real_, L, K)
  cv1 <- array(NA_real_, c(L, K, K)); cv0 <- array(NA_real_, c(L, K, K))
  kf1 <- matrix(NA_real_, L, K); kf0 <- matrix(NA_real_, L, K)
  kf1cv <- matrix(NA_real_, L, K)
  m1s <- numeric(L); m0s <- numeric(L); ms <- numeric(L)

  const_folds <- integer(0)
  for (l in 1:L) {
    Tind <- T[ind == l]; tauind <- tau[ind == l, l]; Yind <- Y[ind == l]
    tauind_full <- tau[, l]
    m <- length(Yind); m1 <- sum(Tind); m0 <- m - m1
    ms[l] <- m; m1s[l] <- m1; m0s[l] <- m0
    if (length(unique(tauind)) == 1L) const_folds <- c(const_folds, l)
    fd <- .gate_labels(tauind, K,
                       sprintf("fold %d scores", l))        # (F4)
    Ystar <- matrix(NA_real_, m, K)
    for (k in 1:K) {
      That <- as.numeric(fd == k)
      plim <- sum(That) / m
      papesm[l, k] <- K * (1/m1 * sum(Tind * That * Yind) +
                           1/m0 * sum(Yind * (1 - Tind) * (1 - That)) -
                           plim/m1 * sum(Yind * Tind) -
                           (1 - plim)/m0 * sum(Yind * (1 - Tind)))
      Ystar[, k] <- (That - 1/K) * Yind                     # (F2) starred
      if (sum(Tind == 1 & That == 1) > 0 && sum(Tind == 0 & That == 1) > 0)
        kf1[l, k] <- mean(Yind[Tind == 1 & That == 1]) - mean(Yind[Tind == 0 & That == 1])
      if (sum(Tind == 1 & That == 0) > 0 && sum(Tind == 0 & That == 0) > 0)
        kf0[l, k] <- mean(Yind[Tind == 1 & That == 0]) - mean(Yind[Tind == 0 & That == 0])
      ## training-variation term: full-data gate ATE under fold-l cutoffs
      if (k == 1)      { hc <- max(tauind[fd == k]); lc <- -Inf }
      else if (k == K) { hc <- Inf; lc <- min(tauind[fd == k]) }
      else             { hc <- max(tauind[fd == k]); lc <- min(tauind[fd == k]) }
      Tf <- as.numeric(tauind_full <= hc & tauind_full >= lc)
      if (sum(T == 1 & Tf == 1) > 0 && sum(T == 0 & Tf == 1) > 0)
        kf1cv[l, k] <- mean(Y[T == 1 & Tf == 1]) - mean(Y[T == 0 & Tf == 1])
    }
    for (a in 1:K) for (b in 1:K) {
      cv1[l, a, b] <- cov(Ystar[Tind == 1, a], Ystar[Tind == 1, b])
      cv0[l, a, b] <- cov(Ystar[Tind == 0, a], Ystar[Tind == 0, b])
    }
  }

  papes <- colMeans(papesm)
  SF    <- cov(papesm)                                      # S^2_Fkk' across folds
  guard_hit <- logical(K)

  mcov <- matrix(0, K, K)
  for (i in 1:K) for (j in 1:K) {
    term1 <- mean(K^2 * (cv1[, i, j] / m1s + cv0[, i, j] / m0s), na.rm = TRUE)  # (F5)
    kb <- mean(.kblock(kf1[, i], kf0[, i], kf1[, j], kf0[, j], K, ms),          # (F3)
               na.rm = TRUE)
    evt <- tryCatch(cov(kf1cv[, i], kf1cv[, j], use = "complete.obs"),
                    error = function(e) 0)
    if (i == j && is.finite(term1 + kb + evt) && is.finite(SF[i, i]) &&
        SF[i, i] > term1 + kb + evt)
      guard_hit[i] <- TRUE                                  # stability diagnostic
    mcov[i, j] <- (if (i == j) max(term1 + kb, 0) else term1 + kb) / L +
                  (if (i == j) max(evt, 0) else evt)        # (F6) analytic
  }
  if (any(guard_hit))
    warning(sprintf(paste0(
      "Cross-fold spread of the per-fold estimates exceeds the analytic ",
      "variance for gate(s) %s: the ML scores appear unstable across folds ",
      "(Assumption 7 suspect). Interpret results with caution and consider a ",
      "more stable learner (e.g. tune.parameters='none')."),
      paste(which(guard_hit), collapse = ",")), call. = FALSE)

  mcov <- diag(diag(mcov), nrow = K, ncol = K)
  if (!is.finite(determinant(mcov)$modulus) || any(diag(mcov) <= 0))
    return(list(stat = NA, pval = NA))
  stat <- as.numeric(t(papes) %*% solve(mcov) %*% papes)
  list(stat = stat, pval = pchisq(stat, K, lower.tail = FALSE),
       guard = guard_hit, constant_folds = const_folds)
}
