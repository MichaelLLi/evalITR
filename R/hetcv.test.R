#' The Heterogeneity Test for Grouped Average Treatment Effects (GATEs) under Cross Validation in Randomized Experiments
#'
#' This function calculates statistics related to the test of heterogeneous treatment effects across groups under cross-validation.
#'
#' The details of the methods for this design are given in Imai and Li (2022).
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample.
#' @param tau A vector of the unit-level continuous score. Conditional Average Treatment Effect is one possible measure.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @param ind A vector of integers (between 1 and number of folds inclusive) indicating which testing set does each sample belong to.
#' @param ngates The number of groups to separate the data into. The groups are determined by \code{tau}. Default is 5.
#' @return A list that contains the following items: \item{stat}{The estimated
#' statistic for the test of heterogeneity under cross-validation.} \item{pval}{The p-value of the null
#' hypothesis (that the treatment effects are homogeneous)}
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' tau = matrix(c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),nrow = 8, ncol = 2)
#' Y = c(4,5,0,2,4,1,-4,3)
#' ind = c(rep(1,4),rep(2,4))
#' hettestlist <- hetcv.test(T,tau,Y,ind,ngates=2)
#' hettestlist$stat
#' hettestlist$pval
#' @author Michael Lingzhi Li, Operations Research Center, Massachusetts Institute of Technology
#' \email{mili@hbs.edu}, \url{http://michaellz.com};
#' @references Imai and Li (2022). \dQuote{Statistical Inference for Heterogeneous Treatment Effects Discovered by Generic Machine Learning in Randomized Experiments},
#' @keywords evaluation
#' @export hetcv.test
hetcv.test<- function(T, tau, Y, ind, ngates = 5) {
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
  papesm = matrix(0,nfolds,ngates)
  Sfp1s = as.list(numeric(ngates))
  Sfp0s = as.list(numeric(ngates))
  cov1s = matrix(0,ngates,ngates)
  cov0s = matrix(0, ngates, ngates)
  kf1t = matrix(NA,nfolds,ngates)
  kf0t = matrix(NA,nfolds,ngates)
  vargts = numeric(ngates)
  kf1cv = matrix(NA,nfolds,ngates)
  mcov = matrix(0, nrow = ngates, ncol = ngates)
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
      papesm[i,j] = ngates * (1/n1*sum(Tind*That*Yind)+1/n0*sum(Yind*(1-Tind)*(1-That))-plim/n1*sum(Yind*Tind)-(1-plim)/n0*sum(Yind*(1-Tind)))
      Sfp1s[[j]] = (That*Yind)[Tind==1]
      Sfp0s[[j]] = (That*Yind)[Tind==0]
      if (length(Yind[Tind==1 & That==1])>0 & length(Yind[Tind==0 & That==1])>0) {
        kf1t[i,j] = (mean(Yind[Tind==1 & That==1])-mean(Yind[Tind==0 & That==1]))
      }
      if (length(Yind[Tind==1 & That==0])>0 & length(Yind[Tind==0 & That==0])>0) {
        kf0t[i,j] =(mean(Yind[Tind==1 & That==0])-mean(Yind[Tind==0 & That==0]))
      }
    }
    for (i in 1:ngates) {
      for (j in 1:ngates) {
        cov1s[i,j] = cov1s[i,j] + cov(Sfp1s[[i]],Sfp1s[[j]]) / nfolds
        cov0s[i,j] = cov0s[i,j] + cov(Sfp0s[[i]],Sfp0s[[j]]) / nfolds
      }
    }
  }

  papes = colMeans(papesm)
  for (i in 1:ngates) {
    for (j in 1:ngates) {
      kf1i2 = mean(kf1t[,i]^2, na.rm = TRUE)
      kf1j2 = mean(kf1t[,j]^2, na.rm = TRUE)
      kf11ij = mean(kf1t[,i]*kf1t[,j], na.rm = TRUE)
      kf10ii = mean(kf1t[,i]*kf0t[,i], na.rm = TRUE)
      kf10jj = mean(kf1t[,j]*kf0t[,j], na.rm = TRUE)
      Sfp = cov(papesm[,i],papesm[,j])
      extra_var_term = cov(kf1cv[,i], kf1cv[,j], use = "complete.obs")
      vargtsij = ngates ^ 2 * ((cov1s[i,j]) / n1 + (cov0s[i,j]) / n0) +
        1/ (ngates * (n - 1)) *((ngates - 1)*(kf1i2-kf10ii+kf1j2-kf10jj) - ngates * (ngates - 1)* kf11ij) + extra_var_term
      mcov[i,j] = vargtsij - (nfolds - 1) / nfolds * min(vargtsij, Sfp)
    }
  }
  mcov = diag(diag(mcov), nrow = ngates, ncol = ngates)
  if (is.finite(determinant(mcov)$modulus)) {
    stat = t(papes) %*% solve(mcov) %*% papes
    return(list(stat=stat,pval=pchisq(stat, ngates, lower.tail = FALSE)))
  } else {
    return(list(stat=NA,pval=NA))
  }
}
