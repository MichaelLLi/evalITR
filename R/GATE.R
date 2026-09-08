#' Estimation of the Grouped Average Treatment Effects (GATEs) in Randomized Experiments
#'
#' Estimates grouped average treatment effects from a continuous score.
#'
#' @param T Binary treatment indicator (0 or 1).
#' @param tau Continuous score vector.
#' @param Y Outcome vector.
#' @param ngates Number of groups (at least 2).
#' @param centered Whether to center outcomes before estimation.
#' @return A list with group estimates \code{gate} and standard errors \code{sd},
#' ordered by increasing score.
#' @details
#' Standard errors account for estimated group cutoffs and assume continuous
#' treatment effects at the boundaries. Ties are broken randomly; use
#' \code{set.seed()} for reproducibility. Both arms need at least two
#' observations. Empty group arms return \code{NA} standard errors.
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' tau = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7)
#' Y = c(4,5,0,2,4,1,-4,3)
#' gatelist <- GATE(T,tau,Y,ngates=2)
#' gatelist$gate
#' gatelist$sd
#' @author Michael Lingzhi Li, Technology and Operations Management, Harvard Business School
#' \email{mili@hbs.edu}, \url{https://www.michaellz.com/};
#' @references Imai and Li (2022). \dQuote{Statistical Inference for Heterogeneous Treatment Effects Discovered by Generic Machine Learning in Randomized Experiments},
#' @keywords evaluation
#' @export GATE
GATE <- function(T, tau, Y, ngates = 5, centered = FALSE) {
  fit <- .gate_fit(T, tau, Y, ngates = ngates, centered = centered)
  list(gate = fit$estimate, sd = sqrt(pmax(diag(fit$covariance), 0)))
}
