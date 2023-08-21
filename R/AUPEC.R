#' Estimation of the Area Under Prescription Evaluation Curve (AUPEC) in Randomized Experiments
#'
#' This function estimates AUPEC. The details of the methods for this design are given in Imai and Li (2019).
#'
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample.
#' @param tau A vector of the unit-level continuous score for treatment assignment. We assume those that have tau<0 should
#' not have treatment. Conditional Average Treatment Effect is one possible measure.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @param centered If \code{TRUE}, the outcome variables would be centered before processing. This minimizes
#' the variance of the estimator. Default is \code{TRUE}.
#' @return A list that contains the following items: \item{aupec}{The estimated
#' Area Under Prescription Evaluation Curve} \item{sd}{The estimated standard deviation
#' of AUPEC.}\item{vec}{A vector of points outlining the AUPEC curve across each possible budget point for the dataset.
#' Each step increases the budget by 1/n where n is the number of data points. }
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' tau = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7)
#' Y = c(4,5,0,2,4,1,-4,3)
#' aupeclist <- AUPEC(T,tau,Y)
#' aupeclist$aupec
#' aupeclist$sd
#' aupeclist$vec
#' @author Michael Lingzhi Li, Operations Research Center, Massachusetts Institute of Technology
#' \email{mili@hbs.edu}, \url{http://michaellz.com};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export AUPEC
AUPEC <- function (T, tau, Y, centered = TRUE) {
  if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
    stop("T should be binary.")
  }
  if ((length(T)!=length(tau)) | (length(tau)!=length(Y))) {
    stop("All the data should have the same length.")
  }
  if (length(T)==0) {
    stop("The data should have positive length.")
  }
  if (!is.logical(centered)) {
    stop("The centered parameter should be TRUE or FALSE.")
  }
  T=as.numeric(T)
  tau=as.numeric(tau)
  Y=as.numeric(Y)
  if (centered) {
    Y = Y - mean(Y)
  }
  n=length(Y)
  n1=sum(T)
  n0=n-n1
  pf=sum(as.numeric(tau>0))/n
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
    AUPECvec=numeric(n)
    covarsum=0
    for (i in 1:n) {
      cutofftemp=quantile(tau,1-i/n)
      Thatftemp=as.numeric(tau>cutofftemp)
      ThatftempA=as.numeric(tau>max(cutofftemp,0))
      AUPECvec[i]=1/n1*sum(T*ThatftempA*Y)+1/n0*sum(Y*(1-T)*(1-ThatftempA))
      ThatfA=ThatfA+1/n*ThatftempA
      cutofftemp2=quantile(tau,(i-1)/n)
      Thatftemp2=as.numeric(tau>cutofftemp2)
      tempkAf1=mean(Y[T==1 & Thatftemp2==1])-mean(Y[T==0 & Thatftemp2==1])
      tempkAf0=mean(Y[T==1 & Thatftemp==0])-mean(Y[T==0 & Thatftemp==0])
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
    covarsum1=mean(-1/(n^3*(n-1))*sumtemp1[Z]-Z*(n-Z)^2/(n^3*(n-1))*kAf1[Z]*kAf0[Z]-2/(n^4*(n-1))*tempMsum[Z]-Z^2*(n-Z)^2/(n^4*(n-1))*kAf1[Z]^2
                   -2*(n-Z)^2/(n^4*(n-1))*kAf1[Z]*sumtemp2[Z]+1/n^4*sumtemp3[Z])
    covarsum2=var(1/n*(sumtemp2[Z]/n+(n-Z)*Z/n*kAf1[Z]))
    ThatfA2=ThatfA-1/2
    SfA1=var((ThatfA2*Y)[T==1])
    SfA0=var((ThatfA2*Y)[T==0])
    varfA=SfA1/n1+SfA0/n0+covarsum1+covarsum2
    AUPEC=1/n1*sum(T*ThatfA*Y)+1/n0*sum(Y*(1-T)*(1-ThatfA))-0.5/n1*sum(T*Y)-0.5/n0*sum((1-T)*Y)
    return(list(aupec=AUPEC,sd=sqrt(max(varfA,0)),vec=AUPECvec))
  } else {
    AUPEC=1/n0*sum(Y*(1-T))-0.5/n1*sum(T*Y)-0.5/n0*sum((1-T)*Y)
    AUPECvec=numeric(n)
    AUPECvec[]=1/n0*sum(Y*(1-T))
    return(list(aupec=AUPEC,sd=0,vec=AUPECvec))
  }
}

