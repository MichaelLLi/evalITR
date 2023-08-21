#' Estimation of the Population Average Prescription Difference in Randomized Experiments
#'
#' This function estimates the Population Average Prescription Difference with a budget
#' constraint. The details of the methods for this design are given in Imai and Li (2019).
#'
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample.
#' @param Thatfp A vector of the unit-level binary treatment that would have been assigned by the
#' first individualized treatment rule. Please ensure that the percentage of treatment units of That is lower than the budget constraint.
#' @param Thatgp A vector of the unit-level binary treatment that would have been assigned by the
#' second individualized treatment rule. Please ensure that the percentage of treatment units of That is lower than the budget constraint.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @param budget The maximum percentage of population that can be treated under the
#' budget constraint. Should be a decimal between 0 and 1.
#' @param centered If \code{TRUE}, the outcome variables would be centered before processing. This minimizes
#' the variance of the estimator. Default is \code{TRUE}.
#' @return A list that contains the following items: \item{papd}{The estimated
#' Population Average Prescription Difference} \item{sd}{The estimated standard deviation
#' of PAPD.}
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' That = c(0,1,1,0,0,1,1,0)
#' That2 = c(1,0,0,1,1,0,0,1)
#' Y = c(4,5,0,2,4,1,-4,3)
#' papdlist <- PAPD(T,That,That2,Y,budget = 0.5)
#' papdlist$papd
#' papdlist$sd
#' @author Michael Lingzhi Li, Technology and Operations Management, Harvard Business School
#' \email{mili@hbs.edu}, \url{https://www.michaellz.com/};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export PAPD
PAPD <- function (T, Thatfp,Thatgp , Y, budget, centered = TRUE) {
  if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
    stop("T should be binary.")
  }
  if (!(identical(as.numeric(Thatfp),as.numeric(as.logical(Thatfp))))) {
    stop("Thatfp should be binary.")
  }
  if (!(identical(as.numeric(Thatgp),as.numeric(as.logical(Thatgp))))) {
    stop("Thatgp should be binary.")
  }
  if ((budget<0) | (budget>1)) {
    stop("Budget constraint should be between 0 and 1")
  }
  if ((length(T)!=length(Thatfp)) | (length(Thatfp)!=length(Thatgp)) | (length(Thatgp)!=length(Y))) {
    stop("All the data should have the same length.")
  }
  if (length(T)==0) {
    stop("The data should have positive length.")
  }
  if ((sum(Thatfp)>floor(length(T)*budget)+1) | (sum(Thatgp)>floor(length(T)*budget)+1)) {
    stop("The proportion of treated units in Thatfp or Thatgp should be below or equal to budget.")
  }
  if (!is.logical(centered)) {
    stop("The centered parameter should be TRUE or FALSE.")
  }
  T=as.numeric(T)
  Thatfp=as.numeric(Thatfp)
  Thatgp=as.numeric(Thatgp)
  Y=as.numeric(Y)
  if (centered) {
    Y = Y - mean(Y)
  }
  n=length(Y)
  n1=sum(T)
  n0=n-n1
  SAPEfp=1/n1*sum(T*Thatfp*Y)+1/n0*sum(Y*(1-T)*(1-Thatfp))-budget/n1*sum(Y*T)-(1-budget)/n0*sum(Y*(1-T))
  SAPEgp=1/n1*sum(T*Thatgp*Y)+1/n0*sum(Y*(1-T)*(1-Thatgp))-budget/n1*sum(Y*T)-(1-budget)/n0*sum(Y*(1-T))
  Sfp1=var(((Thatfp-budget)*Y)[T==1])
  Sfp0=var(((Thatfp-budget)*Y)[T==0])
  kf1=mean(Y[T==1 & Thatfp==1])-mean(Y[T==0 & Thatfp==1])
  kf0=mean(Y[T==1 & Thatfp==0])-mean(Y[T==0 & Thatfp==0])
  PAPD=SAPEfp-SAPEgp
  Sfgp1=var(((Thatfp-Thatgp)*Y)[T==1])
  Sfgp0=var(((Thatfp-Thatgp)*Y)[T==0])
  kg1=mean(Y[T==1 & Thatgp==1])-mean(Y[T==0 & Thatgp==1])
  kg0=mean(Y[T==1 & Thatgp==0])-mean(Y[T==0 & Thatgp==0])
  varfgp=Sfgp1/n1+Sfgp0/n0-floor(n*budget)*(n-floor(n*budget))/(n^2*(n-1))*(kf1^2+kg1^2)+
    2*floor(n*budget)*max(floor(n*budget),n-floor(n*budget))/(n^2*(n-1))*abs(kf1*kg1)
  return(list(papd=PAPD,sd=sqrt(max(varfgp,0))))
}
