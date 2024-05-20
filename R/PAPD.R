#' Estimation of the Population Average Prescription Difference in Randomized Experiments
#'
#' This function estimates the Population Average Prescription Difference with a budget
#' constraint. The details of the methods for this design are given in Imai and Li (2019).
#'
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample. Both binary treatment and multi-value treatment are supported.
#' @param Thatfp A vector of the unit-level binary treatment that would have been assigned by the individualized treatment rule. Multi-value treatment is also supported. Please ensure that the percentage of treatment units of Thatfp is lower than the budget constraint.
#' @param Thatgp A vector of the unit-level binary treatment that would have been assigned by the second individualized treatment rule. Multi-value treatment is also supported. 
#' Please ensure that the percentage of treatment units of That is lower than the budget constraint.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @param budget The maximum percentage of population that can be treated under the
#' budget constraint. Should be a decimal between 0 and 1.
#' @param centered If \code{TRUE}, the outcome variables would be centered before processing. This minimizes
#' the variance of the estimator. Default is \code{TRUE}.
#' @return A list that contains the following items: \item{papd}{The estimated
#' Population Average Prescription Difference} \item{sd}{The estimated standard deviation
#' of PAPD.}
#' @examples
#' # Example 1: Binary treatment
#' T = c(1,0,1,0,1,0,1,0)
#' That = c(0,1,1,0,0,1,1,0)
#' That2 = c(1,0,0,1,1,0,0,1)
#' Y = c(4,5,0,2,4,1,-4,3)
#' papdlist <- PAPD(T,That,That2,Y,budget = 0.5)
#' papdlist$papd
#' papdlist$sd
#' 
#' Example 2: Multi-value treatment
#' T = c(1,2,3,0,1,0,3,2)
#' That = c(3,2,1,3,2,0,3,1)
#' That2 = c(2,0,3,3,2,3,1,2)
#' Y = c(4,5,0,2,4,1,-4,3)
#' papdlist <- PAPD(T,That,That2,Y,budget = 0.5)
#' papdlist$papd
#' papdlist$sd
#' 
#' @author Michael Lingzhi Li, Technology and Operations Management, Harvard Business School
#' \email{mili@hbs.edu}, \url{https://www.michaellz.com/};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export PAPD
PAPD <- function (T, Thatfp,Thatgp , Y, budget, centered = TRUE) {

  # check whether the treatment is binary or multi-value
  if (length(unique(T)) == 2) {
    is_binary = TRUE
  } else {
    is_binary = FALSE
  }

  # binary treatment input check
  if(is_binary){
    if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
      stop("T should be binary.")
    }
    if (!(identical(as.numeric(Thatfp),as.numeric(as.logical(Thatfp))))) {
      stop("Thatfp should be binary.")
    }
    if (!(identical(as.numeric(Thatgp),as.numeric(as.logical(Thatgp))))) {
      stop("Thatgp should be binary.")
    }
    if ((sum(Thatfp)>floor(length(T)*budget)+1) | (sum(Thatgp)>floor(length(T)*budget)+1)) {
      stop("The proportion of treated units in Thatfp or Thatgp should be below or equal to budget.")
    }    
  }

  # convert Thatfp and Thatgp into a matrix
  if(!is_binary){
    Thatfp.m = sapply(unique(Thatfp[Thatfp!= 0]), function(x) as.numeric(Thatfp == x))
    Thatgp.m = sapply(unique(Thatgp[Thatgp!= 0]), function(x) as.numeric(Thatgp == x))
  }

  # multi-value treatment input check
  if(!is_binary){
    if (!is.vector(T)) {
      stop("T should be a vector.")
    }

    # convert Thatfp and Thatgp into a matrix for budget check
    Thatfp.m = sapply(unique(Thatfp[Thatfp!= 0]), function(x) as.numeric(Thatfp == x))
    Thatgp.m = sapply(unique(Thatgp[Thatgp!= 0]), function(x) as.numeric(Thatgp == x))

    if (any(colSums(Thatfp.m)>floor(length(T)*budget)+1) | any(colSums(Thatgp.m)>floor(length(T)*budget)+1)) {
      stop("The proportion of treated units in Thatfp or Thatgp should be below or equal to budget.")
    }  
  }

  # general input check
  if ((length(T)!=length(Thatfp)) | (length(Thatfp)!=length(Thatgp)) | (length(Thatgp)!=length(Y))) {
    stop("All the data should have the same length.")
  }
  if ((budget<0) | (budget>1)) {
    stop("Budget constraint should be between 0 and 1")
  }  
  if (!is.logical(centered)) {
    stop("The centered parameter should be TRUE or FALSE.")
  }
  if (length(T)==0) {
    stop("The data should have positive length.")
  }
  if (centered) {
    Y = Y - mean(Y)
  }

  # binary treatment -------------------------
  if(is_binary){
    T=as.numeric(T)
    Thatfp=as.numeric(Thatfp)
    Thatgp=as.numeric(Thatgp)
    Y=as.numeric(Y)
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
    sd = sqrt(max(varfgp,0))
    
    return(list(papd=PAPD,sd=sd))
  }

  # multi-value treatment -------------------------

  if(!is_binary){

    n_treatment = length(unique(T)) 
    
    # create empty lists to store the results
    vec1 = rep(0,n_treatment)
    vec2 = rep(0,n_treatment)
    n = length(Y)

    # loop through each version of treatment
    for(j in unique(T)){
      T_j = (T==j)*1
      Thatfp_j = (Thatfp == j)*1
      Thatgp_j = (Thatgp == j)*1
      Y = as.numeric(Y)
      n1 = sum(T_j)

      # PAPD estimation
      SAPEfp_left = 1/n1*sum(T_j*Thatfp_j*Y)
      SAPEfp_right = sum(budget/n1*Y*T_j)


      SAPEgp_left = 1/n1*sum(T_j*Thatgp_j*Y)
      SAPEgp_right = sum(budget/n1*Y*T_j)

      vec1[j] = SAPEfp_left - SAPEfp_right
      vec2[j] = SAPEgp_left - SAPEgp_right
    }

    # calculate PAPD
    papd_vec = sum(vec1) - sum(vec2)
      
    # # variance estimation
    # Sfp1=var(((Thatfp_j-budget)*Y)[T_j==1])
    # Sfp0=var(((Thatfp_j-budget)*Y)[T_j==0])
    # kf1=mean(Y[T_j==1 & Thatfp_j==1])-mean(Y[T_j==0 & Thatfp_j==1])
    # kf0=mean(Y[T_j==1 & Thatfp_j==0])-mean(Y[T_j==0 & Thatfp_j==0])
    # Sfgp1=var(((Thatfp_j-Thatgp_j)*Y)[T_j==1])
    # Sfgp0=var(((Thatfp_j-Thatgp_j)*Y)[T_j==0])
    # kg1=mean(Y[T_j==1 & Thatgp_j==1])-mean(Y[T_j==0 & Thatgp_j==1])
    # kg0=mean(Y[T_j==1 & Thatgp_j==0])-mean(Y[T_j==0 & Thatgp_j==0])
    # varfgp=Sfgp1/n1+Sfgp0/n0-floor(n*budget)*(n-floor(n*budget))/(n^2*(n-1))*(kf1^2+kg1^2)+
    #   2*floor(n*budget)*max(floor(n*budget),n-floor(n*budget))/(n^2*(n-1))*abs(kf1*kg1)
    # sd = sqrt(max(varfgp,0))

    return(list(papd=papd_vec,sd=0))
  }
}
