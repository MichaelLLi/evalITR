#' Estimation of the Population Average Prescription Difference in Randomized Experiments Under Cross Validation
#'
#' This function estimates the Population Average Prescription Difference with a budget constaint under cross validation. The details of the methods for this design are given in Imai and Li (2019).
#'
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample.
#' @param Thatfp A matrix where the \code{i}th column is the unit-level binary treatment that would have been assigned by the first
#' individualized treatment rule generated in the \code{i}th fold. Please ensure
#' that the percentage of treatment units of That is lower than the budget constraint.
#' @param Thatgp A matrix where the \code{i}th column is the unit-level binary treatment that would have been assigned by the second
#' individualized treatment rule generated in the \code{i}th fold. Please ensure
#' that the percentage of treatment units of That is lower than the budget constraint.
#' @param Y The outcome variable of interest.
#' @param ind A vector of integers (between 1 and number of folds inclusive) indicating which testing set does each sample belong to.
#' @param budget The maximum percentage of population that can be treated under the
#' budget constraint. Should be a decimal between 0 and 1.
#' @param centered If \code{TRUE}, the outcome variables would be centered before processing. This minimizes
#' the variance of the estimator. Default is \code{TRUE}.
#' @return A list that contains the following items: \item{papd}{The estimated
#' Population Average Prescription Difference.} \item{sd}{The estimated standard deviation
#' of PAPD.}
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' That = matrix(c(0,1,1,0,0,1,1,0,1,0,0,1,1,0,0,1), nrow = 8, ncol = 2)
#' That2 = matrix(c(0,0,1,1,0,0,1,1,1,1,0,0,1,1,0,0), nrow = 8, ncol = 2)
#' Y = c(4,5,0,2,4,1,-4,3)
#' ind = c(rep(1,4),rep(2,4))
#' papdlist <- PAPDcv(T, That, That2, Y, ind, budget = 0.5)
#' papdlist$papd
#' papdlist$sd
#' @author Michael Lingzhi Li, Technology and Operations Management, Harvard Business School
#' \email{mili@hbs.edu}, \url{https://www.michaellz.com/};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export PAPDcv
PAPDcv <- function (T, Thatfp, Thatgp, Y, ind, budget, centered = TRUE) {
  if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
    stop("T should be binary.")
  }
  if (!is.logical(centered)) {
    stop("The centered parameter should be TRUE or FALSE.")
  }
  if (!(identical(as.numeric(Thatfp),as.numeric(as.logical(Thatfp))))) {
    stop("Thatfp should be binary.")
  }
  if (!(identical(as.numeric(Thatgp),as.numeric(as.logical(Thatgp))))) {
    stop("Thatgp should be binary.")
  }
  if ((length(T)!=dim(Thatfp)[1]) | (dim(Thatfp)[1]!=dim(Thatgp)[1]) | (dim(Thatgp)[1]!=length(Y))) {
    stop("All the data should have the same length.")
  }
  if (!is.na(budget) & !(sum(sapply(1:max(ind),function(i) sum(Thatfp[ind==i, i])<=floor(length(T[ind==i])*budget) + 1))==max(ind))) {
    stop("The number of treated units in Thatfp should be below or equal to budget")
  }
  if (!is.na(budget) & !(sum(sapply(1:max(ind),function(i) sum(Thatgp[ind==i, i])<=floor(length(T[ind==i])*budget) + 1))==max(ind))) {
    stop("The number of treated units in Thatgp should be below or equal to budget")
  }
  if (((budget<0) | (budget>1))) {
    stop("Budget constraint should be between 0 and 1")
  }
  if (length(T)==0) {
    stop("The data should have positive length.")
  }
  T=as.numeric(T)
  Thatfp=as.matrix(Thatfp)
  Thatgp=as.matrix(Thatgp)
  Y=as.numeric(Y)
  if (centered) {
    Y = Y - mean(Y)
  }
  nfolds = max(ind)
  n = length(Y)
  n1 = sum(T)
  n0 = n - n1
  papdfold = c()
  Sfgp1 = 0
  Sfgp0 = 0
  kf1 = c()
  kf0 = c()
  kg1 = c()
  kg0 = c()
  for (i in 1:nfolds) {
    output = PAPD(T[ind==i],Thatfp[ind==i,i],Thatgp[ind==i,i], Y[ind==i],budget)
    m = length(T[ind==i])
    m1 = sum(T[ind==i])
    m0 = m - m1
    Sfgp1=Sfgp1 + var(((Thatfp[,i]-Thatgp[,i])*Y)[T==1 & ind==i])/(m1*nfolds)
    Sfgp0=Sfgp0 + var(((Thatfp[,i]-Thatgp[,i])*Y)[T==0 & ind==i])/(m0*nfolds)
    tempf1 = mean(Y[T==1 & Thatfp[,i]==1 & ind ==i])-mean(Y[T==0 & Thatfp[,i]==1 & ind ==i])
    tempf0 = mean(Y[T==1 & Thatfp[,i]==0 & ind ==i])-mean(Y[T==0 & Thatfp[,i]==0 & ind ==i])
    tempg1 = mean(Y[T==1 & Thatgp[,i]==1 & ind ==i])-mean(Y[T==0 & Thatgp[,i]==1 & ind ==i])
    tempg0 = mean(Y[T==1 & Thatgp[,i]==0 & ind ==i])-mean(Y[T==0 & Thatgp[,i]==0 & ind ==i])
    if (!is.nan(tempf1)) {
      kf1 = c(kf1, tempf1)
    }
    if (!is.nan(tempf0)) {
      kf0 = c(kf0, tempf0)
    }
    if (!is.nan(tempf1)) {
      kg1 = c(kg1, tempg1)
    }
    if (!is.nan(tempf0)) {
      kg0 = c(kg0, tempg0)
    }
    papdfold = c(papdfold, output$papd)
  }
  kf1 = mean(kf1)
  kf0 = mean(kf0)
  kg1 = mean(kg1)
  kg0 = mean(kg0)
  mF = n / nfolds
  SF2 = var(papdfold)
  varfp=Sfgp1+Sfgp0-floor(mF*budget)*(mF-floor(mF*budget))/(mF^2*(mF-1))*(kf1^2+kg1^2)+
    2*floor(mF*budget)*max(floor(mF*budget),mF-floor(mF*budget))/(mF^2*(mF-1))*abs(kf1*kg1)
  vartotal = varfp - (nfolds - 1) / nfolds * min(varfp, SF2)
  return(list(papd=mean(papdfold),sd=sqrt(max(vartotal,0))))
}
