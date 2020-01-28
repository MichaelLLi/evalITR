#' Estimation of the Population Average Prescription Effect in Completely Randomized Experiments
#' 
#' This function estimates the Population Average Prescription Effect with and without a budget
#' constraint. The details of the methods for this design are given in Imai and Li (2019).
#' 
#' 
#' 
#' @param T The unit-level binary treatment receipt variable.
#' @param That The unit-level binary treatment that would have been assigned by the 
#' individualized treatment rule.
#' @param Y The outcome variable of interest.
#' @param plim The maximum percentage of population that can be treated under the
#' budget constraint. Should be a decimal between 0 and 1. Default is NA which assumes
#' no budget constraint.
#' @return A list that contains the following items: \item{pape}{The estimated
#' Population Average Prescription Effect.} \item{sd}{The estimated standard deviation
#' of PAPE.}
#' @author Michael Lingzhi Li, Operations Research Center, Massachusetts Institute of Technology
#' \email{mlli@mit.edu}, \url{http://mlli.mit.edu};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export PAPE
PAPE <- function (T, That, Y, plim = NA) {
  if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
    stop("T should be binary.")
  }
  if (!(identical(as.numeric(That),as.numeric(as.logical(That))))) {
    stop("That should be binary.")
  }
  T=as.numeric(T)
  That=as.numeric(That)
  Y=as.numeric(Y)
  if (is.na(plim)) {
    n=length(Y)
    n1=sum(T)
    n0=n-n1
    n1h=sum(That)
    n0h=n-n1h
    probs=sum(That)/n
    SAPE=n/(n-1)*(1/n1*sum(T*That*Y)+1/n0*sum(Y*(1-T)*(1-That))-n1h/n1/n*sum(Y*T)-n0h/n0/n*sum(Y*(1-T)))
    Sf1=var(((That-probs)*Y)[T==1])
    Sf0=var(((That-probs)*Y)[T==0])
    SATE=1/n1*sum(T*Y)-1/n0*(sum((1-T)*Y))
    covarterm=1/n^2*(SAPE^2+2*(n-1)*SAPE*SATE*(2*probs-1)-(1-probs)*probs*n*SATE^2)
    varexp=(n/(n-1))^2*(Sf1/n1+Sf0/n0+covarterm)
    return(list(pape=SAPE,sd=sqrt(varexp)))
  } else {
    if ((plim<0) | (plim>1)) {
      stop("Budget constraint should be between 0 and 1")
    }
    n=length(Y)
    n1=sum(T)
    n0=n-n1
    n1h=sum(That)
    n0h=n-n1h
    SAPEfp=1/n1*sum(T*That*Y)+1/n0*sum(Y*(1-T)*(1-That))-plim/n1*sum(Y*T)-(1-plim)/n0*sum(Y*(1-T))
    Sfp1=var(((That-plim)*Y)[T==1])
    Sfp0=var(((That-plim)*Y)[T==0])
    kf1=mean(Y[T==1 & That==1])-mean(Y[T==0 & That==1])
    kf0=mean(Y[T==1 & That==0])-mean(Y[T==0 & That==0])
    varfp=Sfp1/n1+Sfp0/n0+floor(n*plim)*(n-floor(n*plim))/(n^2*(n-1))*((2*plim-1)*kf1^2-2*plim*kf1*kf0)
    return(list(pape=SAPEfp,sd=sqrt(varfp)))
  }
}
