#'
#' This function use individualized treatment rule  to identify exceptional responders. The details of the methods for this design are given in Imai and Li (2023).
#'
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample.
#' @param tau A vector of the unit-level continuous score. Conditional Average Treatment Effect is one possible measure.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @return A list that contains the following items: \item{rate}{The estimated
#' vector of URATE of length \code{Y}.} \item{sd}{The estimated vector of standard deviation of URATE.}
#' @examples
#' T <- c(1, 0, 1, 0, 1, 0, 1, 0)
#' tau <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)
#' Y <- c(4, 5, 0, 2, 4, 1, -4, 3)
#' ratelist <- URATE(z_test, tau_test1, y_test)
#' ratelist$rate
#' ratelist$sd
#' @author Michael Lingzhi Li, Technology and Operations Management, Harvard Business School
#' \email{mili@hbs.edu}, \url{https://www.michaellz.com/};
#' @references Imai and Li (2022). \dQuote{Statistical Inference for Heterogeneous Treatment Effects Discovered by Generic Machine Learning in Randomized Experiments},
#' @keywords uniform band
#' @import zoo
#' @export URATE
URATE <- function(T, tau, Y) {
  n <- length(Y)
  n1 <- sum(T)
  n0 <- n - n1
  tau <- tau + rnorm(length(tau), sd = 1e-5)
  tau_sort <- sort(tau, decreasing = TRUE, index.return = TRUE)
  temp_sorted <- Y[tau_sort$ix] * (T[tau_sort$ix] * (1 / n1 + 1 / n0) - 1 / n0)
  # urates = cumsum(temp_sorted)
  urates <- cumsum(temp_sorted) * n / seq_along(temp_sorted)
  # Sf1 = var(Y[T==1])
  # Sf0 = var(Y[T==0])
  # vartotal = Sf1 / n1+Sf0 / n0

  # vargts = numeric(n)
  Y1_ranked <- Y[tau_sort$ix][T[tau_sort$ix] == 1]
  Y0_ranked <- Y[tau_sort$ix][T[tau_sort$ix] == 0]
  EY1 <- mean(Y1_ranked)
  EY0 <- mean(Y0_ranked)
  SY11b <- cumsum(Y1_ranked)
  SY211b <- cumsum(Y1_ranked^2)

  l11b <- seq_along(Y1_ranked)
  l01b <- seq_along(Y0_ranked)

  SY01b <- cumsum(Y0_ranked)
  SY201b <- cumsum(Y0_ranked^2)

  EYThat1 <- cumsum(Y1_ranked) / length(Y1_ranked)
  EYThat1sq <- cumsum(Y1_ranked^2) / length(Y1_ranked)
  EYThat0 <- cumsum(Y0_ranked) / length(Y0_ranked)
  EYThat0sq <- cumsum(Y0_ranked^2) / length(Y0_ranked)
  Sfp1b <- EYThat1sq - EYThat1^2
  Sfp0b <- EYThat0sq - EYThat0^2
  # fill in the values at co‰rrect places
  Sfp1 <- numeric(n)
  Sfp0 <- numeric(n)
  Sfp1[T[tau_sort$ix] == 1] <- Sfp1b
  Sfp0[T[tau_sort$ix] == 0] <- Sfp0b
  Sfp1[T[tau_sort$ix] == 0] <- NA
  Sfp0[T[tau_sort$ix] == 1] <- NA
  Sfp1 <- zoo::na.locf(Sfp1, na.rm = FALSE)
  Sfp1 <- zoo::na.locf(Sfp1, fromLast = TRUE)
  Sfp0 <- zoo::na.locf(Sfp0, na.rm = FALSE)
  Sfp0 <- zoo::na.locf(Sfp0, fromLast = TRUE)
  SY11 <- numeric(n)
  SY01 <- numeric(n)
  SY11[T[tau_sort$ix] == 1] <- SY11b
  SY11[T[tau_sort$ix] == 0] <- NA
  SY01[T[tau_sort$ix] == 1] <- NA
  SY01[T[tau_sort$ix] == 0] <- SY01b
  SY11 <- zoo::na.locf(SY11, na.rm = FALSE)
  SY11 <- zoo::na.locf(SY11, fromLast = TRUE)
  SY01 <- zoo::na.locf(SY01, na.rm = FALSE)
  SY01 <- zoo::na.locf(SY01, fromLast = TRUE)
  l11 <- numeric(n)
  l01 <- numeric(n)
  l11[T[tau_sort$ix] == 1] <- l11b
  l11[T[tau_sort$ix] == 0] <- NA
  l01[T[tau_sort$ix] == 1] <- NA
  l01[T[tau_sort$ix] == 0] <- l01b
  l11 <- zoo::na.locf(l11, na.rm = FALSE)
  l11 <- zoo::na.locf(l11, fromLast = TRUE)
  l01 <- zoo::na.locf(l01, na.rm = FALSE)
  l01 <- zoo::na.locf(l01, fromLast = TRUE)
  kf1 <- SY11 / l11 - SY01 / l01
  SY211 <- numeric(n)
  SY201 <- numeric(n)
  SY211[T[tau_sort$ix] == 1] <- SY211b
  SY211[T[tau_sort$ix] == 0] <- NA
  SY201[T[tau_sort$ix] == 1] <- NA
  SY201[T[tau_sort$ix] == 0] <- SY201b
  SY211 <- zoo::na.locf(SY211, na.rm = FALSE)
  SY211 <- zoo::na.locf(SY211, fromLast = TRUE)
  SY201 <- zoo::na.locf(SY201, na.rm = FALSE)
  SY201 <- zoo::na.locf(SY201, fromLast = TRUE)
  tauij11 <- 1 / (l11 * (l11 - 1)) * (SY11^2 - SY211)
  tauij00 <- 1 / (l01 * (l01 - 1)) * (SY01^2 - SY201)
  tauij01 <- 1 / (l01 * l11) * (SY01 * SY11)
  tauij <- tauij11 - 2 * tauij01 + tauij00
  varrates <- Sfp1 / n1 + Sfp0 / n0 - (seq(n, 1, -1) - 1) / (seq(n, 1, -1)^2 * (n - 1)) * kf1^2
  varrates <- varrates * (n / seq_along(temp_sorted))^2
  return(list(rate = urates, sd = sqrt(pmax(varrates, 0))))
}
