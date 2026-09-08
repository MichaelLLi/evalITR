#' The Heterogeneity Test for Grouped Average Treatment Effects (GATEs) in Randomized Experiments
#'
#' Tests whether treatment effects are equal across score groups.
#'
#' @param T Binary treatment indicator (0 or 1).
#' @param tau Continuous score vector.
#' @param Y Outcome vector.
#' @param ngates Number of groups (at least 2).
#' @param centered Whether to center outcomes before estimation.
#' @return A list with test statistic \code{stat} and p-value \code{pval}.
#' @details
#' See \code{\link{GATE}} for inference details.
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' tau = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7)
#' Y = c(4,5,0,2,4,1,-4,3)
#' hettestlist <- het.test(T,tau,Y,ngates=2)
#' hettestlist$stat
#' hettestlist$pval
#' @author Michael Lingzhi Li, Technology and Operations Management, Harvard Business School
#' \email{mili@hbs.edu}, \url{https://www.michaellz.com/};
#' @references Imai and Li (2022). \dQuote{Statistical Inference for Heterogeneous Treatment Effects Discovered by Generic Machine Learning in Randomized Experiments},
#' @keywords evaluation
#' @export het.test
het.test <- function(T, tau, Y, ngates = 5, centered = TRUE) {
  fit <- .gate_fit(T, tau, Y, NULL, ngates, centered, deviations = TRUE)
  .gate_wald(fit)
}
