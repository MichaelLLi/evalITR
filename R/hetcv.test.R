#' The Heterogeneity Test for Grouped Average Treatment Effects (GATEs) under Cross Validation in Randomized Experiments
#'
#' Tests whether treatment effects are equal across cross-validated score groups.
#'
#' @param T Binary treatment indicator (0 or 1).
#' @param tau A matrix of scores with one column per fold. Column \code{i}
#' contains predictions for all observations from a model trained without fold \code{i}.
#' @param Y Outcome vector.
#' @param ind Integer validation-fold labels starting at 1.
#' @param ngates Number of groups (at least 2).
#' @param centered Whether to center outcomes before estimation.
#' @return A list with test statistic \code{stat} and p-value \code{pval}.
#' @details
#' See \code{\link{GATEcv}} for inference details.
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' tau = matrix(c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),nrow = 8, ncol = 2)
#' Y = c(4,5,0,2,4,1,-4,3)
#' ind = c(rep(1,4),rep(2,4))
#' hettestlist <- hetcv.test(T,tau,Y,ind,ngates=2)
#' hettestlist$stat
#' hettestlist$pval
#' @author Michael Lingzhi Li, Technology and Operations Management, Harvard Business School
#' \email{mili@hbs.edu}, \url{https://www.michaellz.com/};
#' @references Imai and Li (2022). \dQuote{Statistical Inference for Heterogeneous Treatment Effects Discovered by Generic Machine Learning in Randomized Experiments},
#' @keywords evaluation
#' @export hetcv.test
hetcv.test <- function(T, tau, Y, ind, ngates = 5, centered = TRUE) {
  fit <- .gate_fit(T, tau, Y, ind, ngates, centered, deviations = TRUE)
  .gate_wald(fit)
}
