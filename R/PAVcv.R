#' Estimation of the Population Average Value in Randomized Experiments Under Cross Validation
#'
#' This function estimates the Population Average Value. The details of the methods for this design are given in Imai and Li (2019).
#'
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample.
#' @param That A matrix where the \code{i}th column is the unit-level binary treatment that would have been assigned by the
#' individualized treatment rule generated in the \code{i}th fold. If \code{budget} is specified, please ensure
#' that the percentage of treatment units of That is lower than the budget constraint.
#' @param Y The outcome variable of interest.
#' @param ind A vector of integers (between 1 and number of folds inclusive) indicating which testing set does each sample belong to.
#' @param centered If \code{TRUE}, the outcome variables would be centered before processing. This minimizes
#' the variance of the estimator. Default is \code{TRUE}.
#' @return A list that contains the following items: \item{pav}{The estimated
#' Population Average Value.} \item{sd}{The estimated standard deviation
#' of PAV.}
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' That = matrix(c(0,1,1,0,0,1,1,0,1,0,0,1,1,0,0,1), nrow = 8, ncol = 2)
#' Y = c(4,5,0,2,4,1,-4,3)
#' ind = c(rep(1,4),rep(2,4))
#' pavlist <- PAVcv(T, That, Y, ind)
#' pavlist$pav
#' pavlist$sd
#' @author Michael Lingzhi Li, Technology and Operations Management, Harvard Business School
#' \email{mili@hbs.edu}, \url{https://www.michaellz.com/};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export PAVcv
PAVcv <- function (T, That, Y, ind, centered = TRUE) {
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
  if (length(T)==0) {
    stop("The data should have positive length.")
  }
  T=as.numeric(T)
  That=as.matrix(That)
  Y=as.numeric(Y)
  if (centered) {
    Y = Y - mean(Y)
  }
  nfolds = max(ind)
  n = length(Y)
  n1 = sum(T)
  n0 = n - n1
  pavfold = c()
  sdfold = c()
  Sf1 = 0
  Sf0 = 0
  covijtauij = 0
  n1n1 = n1*(n1-1)
  n1n0 = n0*n1
  n0n0 = n0*(n0-1)
  ThatYT1mean = apply(That*Y*T,1,mean)
  ThatYT0mean = apply(That*Y*(1-T),1,mean)
  for (i in 1:nfolds) {
    output = PAV(T[ind==i],That[ind==i,i],Y[ind==i])
    m = length(T[ind==i])
    m1 = sum(T[ind==i])
    m0 = m - m1
    probs=sum(That[ind==i,i])/m
    Sf1=Sf1 + var((That[,i]*Y)[T==1 & ind==i])/(m1*nfolds)
    Sf0=Sf0 + var(((1-That[,i])*Y)[T==0 & ind==i])/(m0*nfolds)
    pavfold = c(pavfold, output$pav)
    sdfold = c(sdfold, output$sd)
    covijtauij = covijtauij + (((sum((That[,i]*Y*T))^2-sum((That[,i]*Y^2*T)))/n1n1 -
                                  2*sum((That[,i]*Y*T))*sum(That[,i]*Y*(1-T))/n1n0 +
                                  (sum((That[,i]*Y*(1-T)))^2-sum((That[,i]*Y^2*(1-T))))/n0n0) -
                                 ((sum(ThatYT1mean)^2-sum(ThatYT1mean^2))/n1n1 -
                                    (2*sum(ThatYT1mean)*sum(ThatYT0mean))/n1n0 +
                                    (sum(ThatYT0mean)^2-sum(ThatYT0mean^2))/n0n0)) / nfolds
  }
  mF = n / nfolds
  SF2 = var(pavfold)
  varcv = Sf1+Sf0
  varexp = (varcv + covijtauij) - (nfolds - 1)/ nfolds * min(SF2,(varcv + covijtauij))
  return(list(pav=mean(pavfold),sd=sqrt(max(varexp,0))))
}
