#' Estimation of the unnormalized Area Under Prescription Evaluation Curve (AUPEC) in Completely Randomized Experiments
#' 
#' This function estimates AUPEC. The details of the methods for this design are given in Imai and Li (2019).
#' 
#' 
#' 
#' @param T The unit-level binary treatment receipt variable.
#' @param tau The unit-level continuous score for treatment assignment. We assume those that have tau<0 should
#' not have treatment. Conditional Average Treatment Effect is one possible measure.
#' @param Y The outcome variable of interest.
#' @return A list that contains the following items: \item{aupec}{The estimated
#' Area Under Prescription Evaluation Curve} \item{sd}{The estimated standard deviation
#' of AUPEC.}
#' @author Michael Lingzhi Li, Operations Research Center, Massachusetts Institute of Technology
#' \email{mlli@mit.edu}, \url{http://mlli.mit.edu};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export AUPEC
AUPEC <- function (T, tau , Y) {
  if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
    stop("T should be binary.")
  }
  T=as.numeric(T)
  Y=as.numeric(Y)
  n=length(Y)
  n1=sum(T)
  n0=n-n1
  pf=sum(as.numeric(tau>0))/n
  Z=rbinom(1e4,n,pf)
  
  ThatfA=numeric(n)
  kAf1=numeric(n)
  kAf0=numeric(n)
  kAf1A=numeric(n)
  kAf1B=numeric(n)
  kAf0A=numeric(n)
  kAf0B=numeric(n)
  covarsum=0
  for (i in 1:n) {
    cutofftemp=quantile(tau,1-i/n)
    Thatftemp=as.numeric(tau>cutofftemp)
    ThatftempA=as.numeric(tau>max(cutofftemp,0))
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
  return(list(aupec=AUPEC,sd=sqrt(varfA)))
}
