#' Estimation of the Area Under Prescription Evaluation Curve (AUPEC) in Randomized Experiments Under Cross Validation
#'
#' This function estimates AUPEC. The details of the methods for this design are given in Imai and Li (2019).
#'
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample.
#' @param tau A matrix where the \code{i}th column is the unit-level continuous score for treatment assignment generated in the \code{i}th fold.
#' @param Y The outcome variable of interest.
#' @param ind A vector of integers (between 1 and number of folds inclusive) indicating which testing set does each sample belong to.
#' @param centered If \code{TRUE}, the outcome variables would be centered before processing. This minimizes
#' the variance of the estimator. Default is \code{TRUE}.
#' @return A list that contains the following items: \item{aupec}{The estimated
#' AUPEC.} \item{sd}{The estimated standard deviation
#' of AUPEC.}
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' tau = matrix(c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9),nrow = 8, ncol = 2)
#' Y = c(4,5,0,2,4,1,-4,3)
#' ind = c(rep(1,4),rep(2,4))
#' aupeclist <- AUPECcv(T, tau, Y, ind)
#' aupeclist$aupec
#' aupeclist$sd
#' @author Michael Lingzhi Li, Operations Research Center, Massachusetts Institute of Technology
#' \email{mlli@mit.edu}, \url{http://mlli.mit.edu};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export AUPECcv
AUPECcv <- function (T, tau, Y, ind, centered = TRUE) {
  if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
    stop("T should be binary.")
  }
  if (!is.logical(centered)) {
    stop("The centered parameter should be TRUE or FALSE.")
  }
  if ((length(T)!=dim(tau)[1]) | (dim(tau)[1]!=length(Y))) {
    stop("All the data should have the same length.")
  }
  if (length(T)==0) {
    stop("The data should have positive length.")
  }
  T=as.numeric(T)
  tau=as.matrix(tau)
  Y=as.numeric(Y)
  if (centered) {
    Y = Y - mean(Y)
  }

  nfolds = max(ind)
  aupecfold = c()
  SfA1 = 0
  SfA0 = 0
  covarsum1 = 0
  covarsum2 = c()
  for (i in 1:nfolds) {
    Tind = T[ind==i]
    tauind = tau[ind==i,i]
    Yind = Y[ind==i]
    n=length(Yind)
    n1=sum(Tind)
    n0=n-n1
    pf=sum(as.numeric(tauind>0))/n
    if (pf>0) {
      Z=rbinom(1e4,n,pf)
      Z=Z[Z>0]
      ThatfA=numeric(n)
      kAf1=numeric(n)
      kAf0=numeric(n)
      kAf1A=numeric(n)
      kAf1B=numeric(n)
      kAf0A=numeric(n)
      kAf0B=numeric(n)
      covarsum=0
      for (i in 1:n) {
        cutofftemp=quantile(tauind,1-i/n)
        Thatftemp=as.numeric(tauind>cutofftemp)
        ThatftempA=as.numeric(tauind>max(cutofftemp,0))
        ThatfA=ThatfA+1/n*ThatftempA
        cutofftemp2=quantile(tauind,(i-1)/n)
        Thatftemp2=as.numeric(tauind>cutofftemp2)
        tempkAf1=mean(Yind[Tind==1 & Thatftemp2==1])-mean(Yind[Tind==0 & Thatftemp2==1])
        tempkAf0=mean(Yind[Tind==1 & Thatftemp==0])-mean(Yind[Tind==0 & Thatftemp==0])
        if (is.nan(tempkAf1)) {
          kAf1[n-i+1]=kAf1[n-i+2]
        } else {
          kAf1[n-i+1]=tempkAf1
        }
        if (is.nan(tempkAf0)) {
          kAf0[i]=kAf0[i-1]
        } else {
          kAf0[i]=tempkAf0
        }
        kAf1A[n-i+1]=(n-i+1)*kAf1[n-i+1]
        kAf1B[n-i+1]=(i-1)*kAf1[n-i+1]
        kAf0A[i]=i*kAf0[i]
        kAf0B[i]=(n-i)*kAf0[i]
      }
      sumtemp1=cumsum(kAf1A*kAf0B)
      sumtemp2=cumsum(kAf1A)
      sumtemp3=cumsum(kAf1A*kAf1B)
      tempM=outer(kAf1A,kAf1B)
      tempM[lower.tri(tempM,diag=TRUE)] <- 0
      tempMsum=cumsum(colSums(tempM))
      covarsum1 = covarsum1 + mean(-1/(n^3*(n-1))*sumtemp1[Z]-Z*(n-Z)^2/(n^3*(n-1))*kAf1[Z]*kAf0[Z]-2/(n^4*(n-1))*tempMsum[Z]-Z^2*(n-Z)^2/(n^4*(n-1))*kAf1[Z]^2
                                   -2*(n-Z)^2/(n^4*(n-1))*kAf1[Z]*sumtemp2[Z]+1/n^4*sumtemp3[Z]) / nfolds
      covarsum2 = c(covarsum2, 1/n*(sumtemp2[Z[1]]/n+(n-Z[1])*Z[1]/n*kAf1[Z[1]]))
      ThatfA2=ThatfA-1/2
      SfA1=SfA1 + var((ThatfA2*Yind)[Tind==1]) / (n1 * nfolds)
      SfA0=SfA0 + var((ThatfA2*Yind)[Tind==0]) / (n0 * nfolds)
      aupecfold = c(aupecfold, 1/n1*sum(Tind*ThatfA*Yind)+1/n0*sum(Yind*(1-Tind)*(1-ThatfA))-0.5/n1*sum(Tind*Yind)-0.5/n0*sum((1-Tind)*Yind))
    } else {
      aupecfold = c(aupecfold, 1/n0*sum(Yind*(1-Tind))-0.5/n1*sum(Tind*Yind)-0.5/n0*sum((1-Tind)*Yind))
    }
  }
  SF2 = var(aupecfold)
  varexp = SfA1 + SfA0 + covarsum1 + var(covarsum2)
  vartotal = varexp - (nfolds - 1) / nfolds * min(varexp, SF2)
  return(list(aupec=mean(aupecfold),sd=sqrt(max(vartotal,0))))
}
