#' Estimation of the Population Average Prescription Effect in Randomized Experiments Under Cross Validation
#'
#' This function estimates the Population Average Prescription Effect with and without a budget
#' constraint. The details of the methods for this design are given in Imai and Li (2019).
#'
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample.
#' @param That A matrix where the \code{i}th column is the unit-level binary treatment that would have been assigned by the
#' individualized treatment rule generated in the \code{i}th fold. If \code{budget} is specified, please ensure
#' that the percentage of treatment units of That is lower than the budget constraint.
#' @param Y The outcome variable of interest.
#' @param ind A vector of integers (between 1 and number of folds inclusive) indicating which testing set does each sample belong to.
#' @param budget The maximum percentage of population that can be treated under the
#' budget constraint. Should be a decimal between 0 and 1. Default is NA which assumes
#' no budget constraint.
#' @param centered If \code{TRUE}, the outcome variables would be centered before processing. This minimizes
#' the variance of the estimator. Default is \code{TRUE}.
#' @return A list that contains the following items: \item{pape}{The estimated
#' Population Average Prescription Effect.} \item{sd}{The estimated standard deviation
#' of PAPE.}
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' That = matrix(c(0,1,1,0,0,1,1,0,1,0,0,1,1,0,0,1), nrow = 8, ncol = 2)
#' Y = c(4,5,0,2,4,1,-4,3)
#' ind = c(rep(1,4),rep(2,4))
#' papelist <- PAPEcv(T, That, Y, ind)
#' papelist$pape
#' papelist$sd
#' @author Michael Lingzhi Li, Operations Research Center, Massachusetts Institute of Technology
#' \email{mili@hbs.edu}, \url{http://michaellz.com};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @importFrom stats var quantile rbinom cov pchisq
#' @importFrom dplyr ntile
#' @importFrom MASS ginv mvrnorm
#' @importFrom quadprog solve.QP
#' @importFrom Matrix nearPD
#' @export PAPEcv
PAPEcv <- function (T, That, Y, ind, budget = NA, centered = TRUE) {
  if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
    stop("T should be binary.")
  }
  if (!is.logical(centered)) {
    stop("The centered parameter should be TRUE or FALSE.")
  }
  if (!(identical(as.numeric(That),as.numeric(as.logical(That))))) {
    stop("That should be binary.")
  }
  if ((length(T)!=dim(That)[1]) | (dim(That)[1]!=length(Y))) {
    stop("All the data should have the same length.")
  }
  if (!is.na(budget) & !(sum(sapply(1:max(ind),function(i) sum(That[ind==i, i])<=floor(length(T[ind==i])*budget)+1))==max(ind))) {
    stop("The number of treated units in That should be below or equal to budget")
  }
  if (!is.na(budget) & ((budget<0) | (budget>1))) {
    stop("Budget constraint should be between 0 and 1")
  }
  if (length(T)==0) {
    stop("The data should have positive length.")
  }
  T=as.numeric(T)
  That=as.matrix(That)
  Y=as.numeric(Y)
  if (centered) {
    Y = Y - mean(Y)
  }
  if (is.na(budget)) {
    nfolds = max(ind)
    n = length(Y)
    n1 = sum(T)
    n0 = n - n1
    papefold = c()
    pF = mean(That)
    tau = 1/n1*sum(T*Y)-1/n0*(sum((1-T)*Y))
    eitaui = sum(That*Y*T)/(n1*nfolds)-sum(That*Y*(1-T))/(n0*nfolds)
    Sf1 = 0
    Sf0 = 0
    covij = 0
    covijtaui = 0
    covijtauij = 0
    n1n1 = n1*(n1-1)
    n1n0 = n0*n1
    n0n0 = n0*(n0-1)
    Thatmean = apply(That,1,mean)
    ThatYT1mean = apply(That*Y*T,1,mean)
    ThatYT0mean = apply(That*Y*(1-T),1,mean)
    for (i in 1:nfolds) {
      output = PAPE(T[ind==i],That[ind==i,i],Y[ind==i])
      m = length(T[ind==i])
      m1 = sum(T[ind==i])
      m0 = m - m1
      probs=sum(That[ind==i,i])/m
      Sf1=Sf1 + var(((That[,i]-probs)*Y)[T==1 & ind==i])/(m1*nfolds)
      Sf0=Sf0 + var(((That[,i]-probs)*Y)[T==0 & ind==i])/(m0*nfolds)
      papefold = c(papefold, output$pape)
      covij = covij + (m-2)*(m-3)/(m-1)^2*tau^2*((sum(That[,i])^2-sum(That[,i])-sum(Thatmean)^2+sum(Thatmean^2))/(n*(n-1))) / nfolds
      covijtaui = covijtaui + 2*(m-2)^2/(m-1)^2*tau*((sum(That[,i])-1)*(sum((That[,i]*Y*T))/((n-1)*n1)-sum((That[,i]*Y*(1-T)))/((n-1)*n0)) -
                                                       ((sum(Thatmean)*sum(ThatYT1mean)-sum(Thatmean*ThatYT1mean))/((n-1)*n1)-
                                                          (sum(Thatmean)*sum(ThatYT0mean)-sum(Thatmean*ThatYT0mean))/((n-1)*n0)))/ nfolds
      covijtauij = covijtauij + (m^2-2*m+2)/(m-1)^2*(((sum((That[,i]*Y*T))^2-sum((That[,i]*Y^2*T)))/n1n1 -
                                                        2*sum((That[,i]*Y*T))*sum(That[,i]*Y*(1-T))/n1n0 +
                                                        (sum((That[,i]*Y*(1-T)))^2-sum((That[,i]*Y^2*(1-T))))/n0n0) -
                                                       ((sum(ThatYT1mean)^2-sum(ThatYT1mean^2))/n1n1 -
                                                          (2*sum(ThatYT1mean)*sum(ThatYT0mean))/n1n0 +
                                                          (sum(ThatYT0mean)^2-sum(ThatYT0mean^2))/n0n0)) / nfolds
    }
    mF = n / nfolds
    SF2 = var(papefold)
    covarterm = 1/mF^2*(mean(papefold)^2+2*(mF-1)*mean(papefold)*tau*(2*pF-1)-(1-pF)*pF*mF*tau^2)
    varcv = mF^2/(mF-1)^2*(Sf1+Sf0+covarterm)
    varexp = varcv + covij - covijtaui + covijtauij - (nfolds - 1)/ nfolds * min(SF2, varcv + covij - covijtaui + covijtauij)
    return(list(pape=mean(papefold),sd=sqrt(max(varexp,0))))
  } else {
    nfolds = max(ind)
    n = length(Y)
    n1 = sum(T)
    n0 = n - n1
    papepfold = numeric(nfolds)
    Sfp1 = 0
    Sfp0 = 0
    kf1 = numeric(nfolds)
    kf0 = numeric(nfolds)
    for (i in 1:nfolds) {
      output = PAPE(T[ind==i],That[ind==i,i],Y[ind==i],budget)
      m = length(T[ind==i])
      m1 = sum(T[ind==i])
      m0 = m - m1
      probs=sum(That[ind==i,i])/m
      Sfp1=Sfp1 + var(((That[,i]-budget)*Y)[T==1 & ind==i])/(m1*nfolds)
      Sfp0=Sfp0 + var(((That[,i]-budget)*Y)[T==0 & ind==i])/(m0*nfolds)
      temp1 = mean(Y[T==1 & That[,i]==1 & ind==i])-mean(Y[T==0 & That[,i]==1 & ind==i])
      temp0 = mean(Y[T==1 & That[,i]==0 & ind==i])-mean(Y[T==0 & That[,i]==0 & ind==i])
      if (!is.nan(temp1)) {
        kf1[i] = temp1
      }
      if (!is.nan(temp0)) {
        kf0[i] =temp0
      }
      papepfold[i] = output$pape
    }
    mF = n / nfolds
    SF2 = var(papepfold)
    kf1 = mean(kf1)
    kf0 = mean(kf0)
    varfp=Sfp1+Sfp0+floor(mF*budget)*(mF-floor(mF*budget))/(mF^2*(mF-1))*((2*budget-1)*kf1^2-2*budget*kf1*kf0)
    vartotal = varfp - (nfolds - 1) / nfolds * min(varfp, SF2)
    return(list(papep=mean(papepfold),sd=sqrt(max(vartotal,0))))
  }
}
