#' Estimation of the Population Average Prescription Difference in Completely Randomized Experiments
#' 
#' This function estimates the Population Average Prescription Difference with a budget
#' constraint. The details of the methods for this design are given in Imai and Li (2019).
#' 
#' 
#' 
#' @param T The unit-level binary treatment receipt variable.
#' @param Thatfp The unit-level binary treatment that would have been assigned by the 
#' first individualized treatment rule.
#' @param Thatgp The unit-level binary treatment that would have been assigned by the 
#' second individualized treatment rule.
#' @param Y The outcome variable of interest.
#' @param plim The maximum percentage of population that can be treated under the
#' budget constraint. Should be a decimal between 0 and 1. 
#' @return A list that contains the following items: \item{papd}{The estimated
#' Population Average Prescription Difference} \item{sd}{The estimated standard deviation
#' of PAPD.}
#' @author Michael Lingzhi Li, Operations Research Center, Massachusetts Institute of Technology
#' \email{mlli@mit.edu}, \url{http://mlli.mit.edu};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export PAPD
PAPD <- function (T, Thatfp,Thatgp , Y, plim) {
  if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
    stop("T should be binary.")
  }
  if (!(identical(as.numeric(Thatfp),as.numeric(as.logical(Thatfp))))) {
    stop("Thatfp should be binary.")
  }
  if (!(identical(as.numeric(Thatgp),as.numeric(as.logical(Thatgp))))) {
    stop("Thatgp should be binary.")
  }
  if ((plim<0) | (plim>1)) {
    stop("Budget constraint should be between 0 and 1")
  }
  T=as.numeric(T)
  Thatfp=as.numeric(Thatfp)
  Thatgp=as.numeric(Thatgp)
  Y=as.numeric(Y)
  n=length(Y)
  n1=sum(T)
  n0=n-n1
  SAPEfp=1/n1*sum(T*Thatfp*Y)+1/n0*sum(Y*(1-T)*(1-Thatfp))-plim/n1*sum(Y*T)-(1-plim)/n0*sum(Y*(1-T))
  SAPEgp=1/n1*sum(T*Thatgp*Y)+1/n0*sum(Y*(1-T)*(1-Thatgp))-plim/n1*sum(Y*T)-(1-plim)/n0*sum(Y*(1-T))
  Sfp1=var(((Thatfp-plim)*Y)[T==1])
  Sfp0=var(((Thatfp-plim)*Y)[T==0])
  kf1=mean(Y[T==1 & Thatfp==1])-mean(Y[T==0 & Thatfp==1])
  kf0=mean(Y[T==1 & Thatfp==0])-mean(Y[T==0 & Thatfp==0])
  PAPD=SAPEfp-SAPEgp
  Sfgp1=var(((Thatfp-Thatgp)*Y)[T==1])
  Sfgp0=var(((Thatfp-Thatgp)*Y)[T==0])
  kg1=mean(Y[T==1 & Thatgp==1])-mean(Y[T==0 & Thatgp==1])
  kg0=mean(Y[T==1 & Thatgp==0])-mean(Y[T==0 & Thatgp==0])
  varfgp=Sfgp1/n1+Sfgp0/n0-floor(n*plim)*(n-floor(n*plim))/(n^2*(n-1))*(kf1^2+kg1^2)+
    2*floor(n*plim)*max(floor(n*plim),n-floor(n*plim))/(n^2*(n-1))*abs(kf1*kg1)
  if (varfgp>0) {
    return(list(papd=PAPD,sd=sqrt(varfgp)))
  } else {
    return(list(papd=PAPD,sd=0))
  }
}
