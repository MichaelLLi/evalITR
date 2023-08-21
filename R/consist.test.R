#' The Consistency Test for Grouped Average Treatment Effects (GATEs) in Randomized Experiments
#'
#' This function calculates statistics related to the test of treatment effect consistency across groups.
#'
#' The details of the methods for this design are given in Imai and Li (2022).
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample.
#' @param tau A vector of the unit-level continuous score. Conditional Average Treatment Effect is one possible measure.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @param ngates The number of groups to separate the data into. The groups are determined by \code{tau}. Default is 5.
#' @param nsim Number of Monte Carlo simulations used to simulate the null distributions. Default is 10000.
#' @return A list that contains the following items: \item{stat}{The estimated
#' statistic for the test of consistency} \item{pval}{The p-value of the null
#' hypothesis (that the treatment effects are consistent)}
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' tau = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7)
#' Y = c(4,5,0,2,4,1,-4,3)
#' consisttestlist <- consist.test(T,tau,Y,ngates=5)
#' consisttestlist$stat
#' consisttestlist$pval
#' @author Michael Lingzhi Li, Operations Research Center, Massachusetts Institute of Technology
#' \email{mili@hbs.edu}, \url{http://michaellz.com};
#' @references Imai and Li (2022). \dQuote{Statistical Inference for Heterogeneous Treatment Effects Discovered by Generic Machine Learning in Randomized Experiments},
#' @keywords evaluation
#' @export consist.test
consist.test <- function(T, tau, Y, ngates = 5, nsim = 10000) {
  if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
    stop("T should be binary.")
  }
  if ((length(T)!=length(tau)) | (length(tau)!=length(Y))) {
    stop("All the data should have the same length.")
  }
  if (length(T)==0) {
    stop("The data should have positive length.")
  }
  n = length(Y)
  n1 = sum(T)
  n0 = n-n1
  fd_label = ntile(tau, ngates)
  vargts = numeric(ngates)
  papes = numeric(ngates)
  kf1s = numeric(ngates)
  kf0s = numeric(ngates)
  Sfp1s = as.list(numeric(ngates))
  Sfp0s = as.list(numeric(ngates))
  mcov = matrix(0, nrow = ngates, ncol = ngates)
  for (i in 1:ngates) {
    That = as.numeric(fd_label == i)
    plim = 1 / ngates
    papes[i] = ngates * (1/n1*sum(T*That*Y)+1/n0*sum(Y*(1-T)*(1-That))-plim/n1*sum(Y*T)-(1-plim)/n0*sum(Y*(1-T)))
    Sfp1s[[i]] = (That*Y)[T==1]
    Sfp0s[[i]] = (That*Y)[T==0]
    kf1s[i] = mean(Y[T==1 & That==1])-mean(Y[T==0 & That==1])
    kf0s[i] = mean(Y[T==1 & That==0])-mean(Y[T==0 & That==0])
  }
  for (i in 1:ngates) {
    for (j in 1:ngates) {
      mcov[i,j] = ngates ^ 2 * (cov(Sfp1s[[i]],Sfp1s[[j]]) / n1 + cov(Sfp0s[[i]],Sfp0s[[j]]) / n0) +
        1/ (ngates * (n - 1)) *((ngates - 1)*(kf1s[i]^2-kf1s[i]*kf0s[i]+kf1s[j]^2-kf1s[j]*kf0s[j]) - ngates * (ngates - 1)* kf1s[i] * kf1s[j])
    }
  }
  mcov[is.nan(mcov)] = 0
  mcov = as.matrix(nearPD(mcov, eig.tol = 1e-4)$mat)
  if (is.finite(determinant(mcov)$modulus)) {
    mcov_inv = as.matrix(nearPD(ginv(mcov))$mat)
    rsamples = mvrnorm(n=nsim, numeric(ngates), ginv(mcov_inv))
    A = matrix(data = 0, nrow= ngates-1, ncol = ngates)
    for (i in 1:(ngates-1)) {
      A[i,i] = -1
      A[i,i+1] = 1
    }
    values = numeric(nsim)
    for (i in 1:nsim) {
      values[i] = (solve.QP(Dmat=mcov_inv,dvec=mcov_inv %*% rsamples[i,],Amat=t(A))$value * 2 + t(rsamples[i,]) %*% mcov_inv %*% rsamples[i,])
    }
    actual_val = (solve.QP(Dmat=mcov_inv,dvec=mcov_inv %*% papes,Amat=t(A))$value * 2 + (t(papes) %*% mcov_inv %*% papes)[1,1])
    return(list(stat=actual_val,pval=1-mean(actual_val>values)))
  } else {
    return(list(stat=NA,pval=NA))
  }
}
