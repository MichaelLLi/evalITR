#' Estimation of the Grouped Average Treatment Effects (GATEs) in Randomized Experiments Under Cross Validation
#'
#' This function estimates the Grouped Average Treatment Effects (GATEs) under cross-validation where the groups are determined by a continuous score. The details of the methods for this design are given in Imai and Li (2022).
#'
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample.
#' @param tau A matrix where the \code{i}th column is the unit-level continuous score for treatment assignment generated in the \code{i}th fold. Conditional Average Treatment Effect is one possible measure.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @param ind A vector of integers (between 1 and number of folds inclusive) indicating which testing set does each sample belong to.
#' @param ngates The number of groups to separate the data into. The groups are determined by \code{tau}. Default is 5.
#' @return A list that contains the following items: \item{gate}{The estimated
#' vector of GATEs under cross-validation of length \code{ngates} arranged in order of increasing \code{tau}.} \item{sd}{The estimated vector of standard deviation
#' of GATEs under cross-validation.}
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' tau = matrix(c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),nrow = 8, ncol = 2)
#' Y = c(4,5,0,2,4,1,-4,3)
#' ind = c(rep(1,4),rep(2,4))
#' gatelist <- GATEcv(T, tau, Y, ind, ngates = 2)
#' gatelist$gate
#' gatelist$sd
#' @author Michael Lingzhi Li, Operations Research Center, Massachusetts Institute of Technology
#' \email{mlli@mit.edu}, \url{http://mlli.mit.edu};
#' @references Imai and Li (2022). \dQuote{Statistical Inference for Heterogeneous Treatment Effects Discovered by Generic Machine Learning in Randomized Experiments},
#' @keywords evaluation
#' @export GATEcv
#'
#'
GATEcv <- function(T, tau, Y, ind, ngates = 5) {
  if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
    stop("T should be binary.")
  }
  if ((length(T)!=dim(tau)[1]) | (dim(tau)[1]!=length(Y))) {
    stop("All the data should have the same length.")
  }
  if (length(T)==0) {
    stop("The data should have positive length.")
  }
  nfolds = max(ind)
  gatesm = matrix(0,nfolds,ngates)
  Sfp1 = numeric(ngates)
  Sfp0 = numeric(ngates)
  Sfpt = numeric(ngates)
  kf1t = matrix(NA,nfolds,ngates)
  vargts = numeric(ngates)
  kf1cv = matrix(NA,nfolds,ngates)
  for (i in 1:nfolds) {
    Tind = T[ind==i]
    tauind = tau[ind==i, i]
    Yind = Y[ind==i]
    tauind_full = tau[, i]
    n = length(Yind)
    n1 = sum(Tind)
    n0 = n-n1
    fd_label = ntile(tauind, ngates)
    for (j in 1:ngates) {
      That = as.numeric(fd_label == j)
      tau_hcutoff = max(tauind[fd_label == j])
      if (j==1) {
        tau_hcutoff = max(tauind[fd_label==j])
        tau_lcutoff = -Inf
      } else if (j==ngates) {
        tau_hcutoff = Inf
        tau_lcutoff = min(tauind[fd_label==j])
      } else {
        tau_hcutoff = max(tauind[fd_label==j])
        tau_lcutoff = min(tauind[fd_label==j])
      }
      That_full = as.numeric((tauind_full <= tau_hcutoff) & (tauind_full >=tau_lcutoff))
      if (length(Y[T==1 & That_full==1])>0 & length(Y[T==0 & That_full==1])>0) {
        kf1cv[i, j] = mean(Y[T==1 & That_full==1])-mean(Y[T==0 & That_full==1])
      }
      plim = sum(That)/ length(That)
      gatesm[i,j] = (1/n1*sum(Tind*That*Yind)+1/n0*sum(Yind*(1-Tind)*(1-That))-1/n0*sum(Yind * (1-Tind)))
      Sfp1[j] = Sfp1[j] + var((That*Yind)[Tind==1]) / (n1 * nfolds)
      Sfp0[j] = Sfp0[j] + var((That*Yind)[Tind==0])/ (n0 * nfolds)
      Sfpt[j] = Sfpt[j] + var(That*Yind * (Tind/(n1/n) - (1-Tind)/(n0/n)))/ (n * nfolds)
      if (length(Yind[Tind==1 & That==1])>0 & length(Yind[Tind==0 & That==1])>0) {
        kf1t[i,j] = (mean(Yind[Tind==1 & That==1])-mean(Yind[Tind==0 & That==1]))
      }
    }
  }
  gates = ngates * colMeans(gatesm)
  for (i in 1:ngates) {
    kf12 = mean(kf1t[,i]^2, na.rm = TRUE)
    Sfp = var(gatesm[,i])
    extra_var_term = var(kf1cv[,i], na.rm=TRUE)
    vargtsi = Sfp1[i] + Sfp0[i] - (ngates -1) / ((ngates ^ 2) * (n-1)) * kf12 + extra_var_term / (ngates * ngates)
    vargts[i] = ngates * ngates * (vargtsi - (nfolds - 1) / nfolds * min(vargtsi, Sfp))
  }
  return(list(gate=gates,sd=sqrt(pmax(vargts,0))))
}
