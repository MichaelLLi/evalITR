#' Estimation of the Grouped Average Treatment Effects (GATEs) in Randomized Experiments
#'
#' This function estimates the Grouped Average Treatment Effects (GATEs) where the groups are determined by a continuous score. The details of the methods for this design are given in Imai and Li (2022).
#'
#'
#'
#' @param T A vector of the unit-level binary treatment receipt variable for each sample.
#' @param tau A vector of the unit-level continuous score. Conditional Average Treatment Effect is one possible measure.
#' @param Y A vector of the outcome variable of interest for each sample.
#' @param ngates The number of groups to separate the data into. The groups are determined by \code{tau}. Default is 5.
#' @return A list that contains the following items: \item{gate}{The estimated
#' vector of GATEs of length \code{ngates} arranged in order of increasing \code{tau}.} \item{sd}{The estimated vector of standard deviation
#' of GATEs.}
#' @examples
#' T = c(1,0,1,0,1,0,1,0)
#' tau = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7)
#' Y = c(4,5,0,2,4,1,-4,3)
#' gatelist <- GATE(T,tau,Y,ngates=5)
#' gatelist$gate
#' gatelist$sd
#' @author Michael Lingzhi Li, Technology and Operations Management, Harvard Business School
#' \email{mili@hbs.edu}, \url{https://www.michaellz.com/};
#' @references Imai and Li (2022). \dQuote{Statistical Inference for Heterogeneous Treatment Effects Discovered by Generic Machine Learning in Randomized Experiments},
#' @keywords evaluation
#' @export GATE
GATE <- function(T, tau, Y, ngates = 5) {
  if (!(identical(as.numeric(T),as.numeric(as.logical(T))))) {
    stop("T should be binary.")
  }
  if ((length(T)!=length(tau)) | (length(tau)!=length(Y))) {
    stop("All the data should have the same length.")
  }
  if (length(T)==0) {
    stop("The data should have positive length.")
  }
  n = length(Y)
  n1 = sum(T)
  n0 = n-n1
  fd_label = ntile(tau, ngates)
  vargts = numeric(ngates)
  gates = numeric(ngates)
  for (i in 1:ngates) {
    That = as.numeric(fd_label == i)
    plim = sum(That)/ length(That)
    gates[i] = ngates * (1/n1*sum(T*That*Y)+1/n0*sum(Y*(1-T)*(1-That))-1/n0*sum(Y * (1-T)))
    Sfp1 = var((That*Y)[T==1])
    Sfp0 = var((That*Y)[T==0])
    if (length(Y[T==1 & That==1])>0 & length(Y[T==0 & That==1])>0) {
      kf1 = mean(Y[T==1 & That==1])-mean(Y[T==0 & That==1])
      vargts[i] = ngates * ngates * (Sfp1 / n1+Sfp0 / n0 - (ngates -1) / ((ngates ^ 2) * (n-1)) * kf1 ^ 2)
    } else {
      vargts[i] = NA
    }
  }
  return(list(gate=gates,sd=sqrt(pmax(vargts,0))))
}
