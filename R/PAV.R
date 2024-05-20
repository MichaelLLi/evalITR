#' Estimation of the Population Average Value in Randomized Experiments
#'
#' This function estimates the Population Average Value. The details of the methods for this design are given in Imai and Li (2019).
#'
#'
#'
#' @param T A vector of the unit-level treatment receipt variable for each sample. Both binary treatment and multi-value treatment are supported.
#' @param That  If the treatment is binary, the input is a vector of the unit-level binary treatment that would have been assigned by the individualized treatment rule. Alternatively, if the treatment is multi-value, the input should be a matrix where the \code{i}th column is the unit-level binary treatment that would have been assigned by the
#' individualized treatment rule generated in the \code{i}th version of treatment.
#'  The number of columns should be equal to the number of versions of the treatment assignment. The number of rows should be equal to the number of observations in the sample. If \code{budget} is specified, please ensure
#' that the percentage of treatment units of That is lower than the budget constraint.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @param centered If \code{TRUE}, the outcome variables would be centered before processing. This minimizes
#' the variance of the estimator. Default is \code{TRUE}.
#' @return A list that contains the following items: \item{pav}{The estimated
#' Population Average Value.} \item{sd}{The estimated standard deviation
#' of PAV.}
#' @examples
#' # Example 1: Binary treatment
#' T = c(1,0,1,0,1,0,1,0)
#' That = c(0,1,1,0,0,1,1,0)
#' Y = c(4,5,0,2,4,1,-4,3)
#' pavlist <- PAV(T,That,Y)
#' pavlist$pav
#' pavlist$sd
#' 
#' # Example 2: Multi-value treatment
#' T = c(1,2,3,0,1,0,3,1)
#' That = matrix(c(0,1,0,0,0,1,1,0,0,0,1,0,0,1,0,0,0,1,1,0,0,1,0,0),ncol=3)
#' Y = c(4,5,0,2,4,1,-4,3)
#' pavlist <- PAV(T,That,Y)
#' pavlist$pav
#' pavlist$sd
#' 
#' @author Michael Lingzhi Li, Technology and Operations Management, Harvard Business School
#' \email{mili@hbs.edu}, \url{https://www.michaellz.com/};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export PAV
PAV <- function (T, That, Y, centered = TRUE) {

  # check whether the treatment is binary or multi-value
  if (is.matrix(That)) {
    is_binary = FALSE
  } else {
    is_binary = TRUE
  }

  # binary treatment input check
  if (is_binary) {
    if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
      stop("T should be binary.")
    }
    if (!(identical(as.numeric(That),as.numeric(as.logical(That))))) {
      stop("That should be binary.")
    }
    if ((length(T)!=length(That)) | (length(That)!=length(Y))) {
      stop("All the data should have the same length.")
    }
  }

  # multi-value treatment input check
  if (!is_binary) {
    if (!is.vector(T)) {
      stop("T should be a vector.")
    }
    if ((length(T)!=nrow(That)) | (length(Y)!=nrow(That))) {
      stop("All the data should have the same length.")
    }
  }

  # general input check
  if (length(T)==0) {
    stop("The data should have positive length.")
  }
  if (!is.logical(centered)) {
    stop("The centered parameter should be TRUE or FALSE.")
  }  
  if (centered) {
    Y = Y - mean(Y)
  }  

  # binary treatment -------------------------
  if(is_binary) {
    T=as.numeric(T)
    That=as.numeric(That)
    Y=as.numeric(Y)
    n=length(Y)
    n1=sum(T)
    n0=n-n1
    # PAV estimation
    SAV=1/n1*sum(T*That*Y)+1/n0*sum(Y*(1-T)*(1-That))
    # variance estimation
    Sf1=var((That*Y)[T==1])
    Sf0=var(((1-That)*Y)[T==0])
    varexp=Sf1/n1+Sf0/n0
    sd = sqrt(max(varexp,0))
    return(list(pav=SAV,sd=sd))
  }

  # multi-value treatment -------------------------
  if (!is_binary) {

    n_treatment = length(unique(T))

    # create empty lists to store the results
    sav_vec = rep(0,n_treatment)
    sd_vec = rep(0,n_treatment)

    # parameters
    Y      = as.numeric(Y)
    n      = length(Y)

    # loop through each treatment
    for (j in unique(T)) {
      T_j    = (T==j)*1
      That_j = That[,j]
      n1     = sum(T_j)

      # PAV estimation
      SAV= 1/n1*sum(T_j*That_j*Y)
      sav_vec[j] = SAV

      # variance estimation
      Sf1=var((That_j*Y)[T_j==1])
      sd_vec[j] = Sf1/n1
    }

    sav = sum(sav_vec)
    varexp = sum(sd_vec)
    sd = sqrt(max(varexp,0))
    
    return(list(pav=sav,sd=sd))

  }
}
