#' Estimation of the Population Average Prescription Effect in Randomized Experiments
#'
#' This function estimates the Population Average Prescription Effect with and without a budget
#' constraint. The details of the methods for this design are given in Imai and Li (2019).
#' @param T A vector of the unit-level treatment receipt variable for each sample. Both binary treatment and multi-value treatment are supported.
#' @param That   A vector of the unit-level binary treatment that would have been assigned by the individualized treatment rule. Multi-value treatment is also supported. 
#'  If \code{budget} is specified, please ensure
#' that the percentage of treatment units of That is lower than the budget constraint.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @param budget The maximum percentage of population that can be treated under the
#' budget constraint. Should be a decimal between 0 and 1. Default is NA which assumes
#' no budget constraint.
#' @param centered If \code{TRUE}, the outcome variables would be centered before processing. This minimizes
#' the variance of the estimator. Default is \code{TRUE}.
#' @return A list that contains the following items: \item{pape}{The estimated
#' Population Average Prescription Effect.} \item{sd}{The estimated standard deviation
#' of PAPE.}
#' @examples
#' # Example 1: Binary treatment
#' T = c(1,0,1,0,1,0,1,0)
#' That = c(0,1,1,0,0,1,1,0)
#' Y = c(4,5,0,2,4,1,-4,3)
#' papelist <- PAPE(T,That,Y)
#' papelist$pape
#' papelist$sd
#' 
#' # Example 2: Multi-value treatment
#' T = c(1,2,3,0,1,0,3,2)
#' That = c(1,2,1,3,2,0,3,1)
#' Y = c(4,5,0,2,4,1,-4,3)
#' papelist <- PAPE(T,That,Y, budget = 0.8)
#' papelist$pape
#' papelist$sd
#' 
#' @author Michael Lingzhi Li, Technology and Operations Management, Harvard Business School
#' \email{mili@hbs.edu}, \url{https://www.michaellz.com/};
#' @references Imai and Li (2019). \dQuote{Experimental Evaluation of Individualized Treatment Rules},
#' @keywords evaluation
#' @export PAPE
PAPE_test <- function (T, That, Y, budget = NA, centered = TRUE) {

  # check whether the treatment is binary or multi-value
  if (length(unique(T)) == 2) {
    is_binary = TRUE
  } else {
    is_binary = FALSE
  }

  # # convert That into a matrix
  # if(!is_binary){
  #   That.m = sapply(unique(That[That!= 0]), function(x) as.numeric(That == x))
  # }

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
    if (!is.na(budget) & (sum(That) > floor(length(T)*budget)+1)) {
      stop("The number of treated units in That should be below or equal to budget")
    }
  }

  # multi-value treatment input check
  if (!is_binary) {
    if (!is.vector(T)) {
      stop("T should be a vector.")
    }
    if ((length(T)!=length(That)) | (length(Y)!=length(That))) {
      stop("All the data should have the same length.")
    }
    # convert That into a matrix for budget constraint check
    That.m = sapply(unique(That[That!= 0]), function(x) as.numeric(That == x))
    if (!is.na(budget) & any(colSums(That.m) > floor(length(T)*budget)+1)) {
      stop("For each version of treatment, the number of treated units in That should be below or equal to budget")
    }    
  }

  # general input check
  if (length(T)==0) {
    stop("The data should have positive length.")
  }  
  if (!is.logical(centered)) {
    stop("The centered parameter should be TRUE or FALSE.")
  }
  if (!is.na(budget) & ((budget<0) | (budget>1))) {
    stop("Budget constraint should be between 0 and 1")
  }
  if (centered) {
    Y = Y - mean(Y)
  }

  # binary treatment ------------------------------ 
  if(is_binary) {
    T=as.numeric(T)
    That=as.numeric(That)
    Y=as.numeric(Y)

    # no budget constraint
    if (is.na(budget)) {
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
      return(list(pape=SAPE,sd=sqrt(max(varexp,0))))
    } else {
      # budget constraint
      n=length(Y)
      n1=sum(T)
      n0=n-n1
      n1h=sum(That)
      n0h=n-n1h
      
      SAPEfp=1/n1*sum(T*That*Y)+1/n0*sum(Y*(1-T)*(1-That))-budget/n1*sum(Y*T)-(1-budget)/n0*sum(Y*(1-T))
      Sfp1=var(((That-budget)*Y)[T==1])
      Sfp0=var(((That-budget)*Y)[T==0])
      kf1=mean(Y[T==1 & That==1])-mean(Y[T==0 & That==1])
      kf0=mean(Y[T==1 & That==0])-mean(Y[T==0 & That==0])
      
      varfp=Sfp1/n1+Sfp0/n0+floor(n*budget)*(n-floor(n*budget))/(n^2*(n-1))*((2*budget-1)*kf1^2-2*budget*kf1*kf0)
      return(list(pape=SAPEfp,sd=sqrt(max(varfp,0))))
    }
  }

  # multi-value treatment ------------------------------

  if(!is_binary){

    n_treatment = length(unique(T))

    # no budget constraint
    if(is.na(budget)){

      # create empty list to store results
      vec = rep(0,n_treatment)
      
      n = length(Y)

    # loop through each treatment
      for(j in unique(T)){

        T_j = (T==j)*1
        That_j = (That == j)*1

        n1 = sum(T==j)
        n1h = sum(That_j)
        probs = sum(That_j)/n
        
        # calculate pape
        sape_left_term = sum(1/n1*T_j*That_j*Y)
        sape_right_term = sum(probs/n1*Y*T_j)

        # store results
        vec[j] = sape_left_term - sape_right_term

      }

      # calculate pape
      SAPE = n/(n-1)*(sum(vec)) 

      # calculate variance
      # Sf1 = var(((That_j-probs)*Y)[T_j==1])
      # Sf0 = var(((That_j-probs)*Y)[T_j==0])
      # SATE = 1/n1*sum(T_j*Y)-1/n0*(sum((1-T_j)*Y))        
      # covarterm = 1/n^2*(SAPE^2+2*(n-1)*SAPE*SATE*(2*probs-1)-(1-probs)*probs*n*SATE^2)
      # varexp = (n/(n-1))^2*(Sf1/n1+Sf0/n0+covarterm)
      # sd = sqrt(max(varexp,0))

      return(list(pape=SAPE,sd=0))      
    }

    # budget constraint
    # if(!is.na(budget)){

    #   n_treatment = length(unique(T)) - 1
      
    #   # create empty list to store results
    #   sape_vec = rep(0,n_treatment)
    #   sd_vec = rep(0,n_treatment)

    #   # loop through each version of treatment
    #   for(j in unique(T[T!=0])){

    #     T_j = (T==j)*1
    #     That_j = (That == j)*1

    #     n = length(Y)
    #     n1 = sum(T==j)
    #     n0 = n-n1
    #     n1h = sum(That_j)
    #     n0h = n-n1h
    #     probs = sum(That_j)/n

    #     # calculate pape
    #     SAPEfp = 1/n1*sum(T_j*That_j*Y)+1/n0*sum(Y*(1-T_j)*(1-That_j))-budget/n1*sum(Y*T_j)-(1-budget)/n0*sum(Y*(1-T_j))

    #     # calculate variance
    #     Sfp1 = var(((That_j-budget)*Y)[T_j==1])
    #     Sfp0 = var(((That_j-budget)*Y)[T_j==0])
    #     kf1 = mean(Y[T_j==1 & That_j==1])-mean(Y[T_j==0 & That_j==1])
    #     kf0 = mean(Y[T_j==1 & That_j==0])-mean(Y[T_j==0 & That_j==0])
    #     varfp = Sfp1/n1+Sfp0/n0+floor(n*budget)*(n-floor(n*budget))/(n^2*(n-1))*((2*budget-1)*kf1^2-2*budget*kf1*kf0)

    #     # store results
    #     sape_vec[j] = SAPEfp
    #     sd_vec[j] = sqrt(max(varfp,0))

    #   }

      return(list(pape=sape_vec,sd=sd_vec))
    }    
  }

}

