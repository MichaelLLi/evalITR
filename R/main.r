#' Evaluate ITR
#' 
#' @param outcome Outcome variable (or a list of outcome variables). Only takes in numeric values for both continous outcomes and binary outcomes (0 or 1).
#' @param treatment Treatment variable 
#' @param data 
#'   A data frame that contains \code{outcome} and \code{treatment}.
#' @param algorithms 
#'   List of machine learning algorithms.  
#' @param plim 
#'   Proportion of treated units.
#' @param n_folds 
#'   Number of cross-validation folds.
#' @param covariates
#'   Covariates included in the model.
#' @import dplyr
#' @importFrom rlang !! sym
#' @export
#' @return An object of \code{itr} class
run_itr <- function(
  outcome,
  treatment, 
  covariates,
  data, 
  algorithms,
  plim,
  n_folds
) {


  ## number of algorithms
  n_alg <- length(algorithms)


  ## some working variables 
  n_df <- nrow(data)
  n_X  <- length(data) - 1
  NFOLDS <- n_folds 

  params <- list(
    n_df = n_df, n_folds = n_folds, n_alg = n_alg
  )


  ## loop over all outcomes
  estimates <- qoi <- vector("list", length = length(outcome))
  for (m in 1:length(outcome)) {

    ## data to use 
    ## rename outcome and treatment variable 
    data_filtered <- data %>% 
      select(Y = !!sym(outcome[m]), Treat = !!sym(treatment), all_of(covariates))
    
    ## create folds 
    treatment_vec <- data_filtered %>% dplyr::pull(Treat)
    folds <- caret::createFolds(treatment_vec, k = NFOLDS)


    ## run 
    estimates[[m]] <- itr_single_outcome(
      data       = data_filtered, 
      algorithms = algorithms, 
      params     = params, 
      folds      = folds,
      plim       = plim
    )

    
    ## format output 
    qoi[[m]] <- compute_qoi(estimates[[m]], algorithms)
    
  }

  out <- list(qoi       = qoi,
              estimates = estimates)
              
  class(out) <- c("itr", class(out))

  return(out)

}


#' Evaluate ITR for Single Outcome
#' 
#' @importFrom purrr map
#' @importFrom dplyr pull
#' @param data A dataset
#' @param algorithms Machine learning algorithms
#' @param params A list of parameters
#' @param folds Number of folds
#' @param plim The maximum percentage of population that can be treated under the budget constraint
itr_single_outcome <- function(
  data, 
  algorithms,
  params, 
  folds,
  plim
) {

  ## obj to store outputs 
  fit_ml <- lapply(1:params$n_alg, function(x) vector("list", length = params$n_folds))
  names(fit_ml) <- algorithms


  Tcv <- dplyr::pull(data, "Treat")
  Ycv <- dplyr::pull(data, "Y") 
  indcv <- rep(0, length(Ycv))


  params$n_tb <- max(table(indcv))

  ## run 

  for (j in seq_len(params$n_folds)) {

    ## ---------------------------------
    ## data split 
    ## ---------------------------------    
    testset  <- data[folds[[j]], ]
    trainset <- data[-folds[[j]], ]
    indcv[folds[[j]]] <- rep(j, nrow(testset))    


    ## ---------------------------------
    ## run ML 
    ## ---------------------------------

    ## prepare data 
    training_data_elements <- create_ml_arguments(
      outcome = "Y", treatment = "Treat", data = trainset
    )

    testing_data_elements <- create_ml_arguments(
      outcome = "Y", treatment = "Treat", data = testset
    )
    
    total_data_elements <- create_ml_arguments(
      outcome = "Y", treatment = "Treat", data = data
    )
    

    ##
    ## run each ML algorithm 
    ## 
    if ("causal_forest" %in% algorithms) {
      fit_ml[["causal_forest"]][[j]] <- run_causal_forest(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params, 
        indcv     = indcv, 
        iter      = j, 
        plim      = plim
      )
    }
    
    if("lasso" %in% algorithms){
      fit_ml[["lasso"]][[j]] <- run_lasso(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params, 
        indcv     = indcv, 
        iter      = j, 
        plim      = plim
      )
    }
    
    if("svm" %in% algorithms){
      fit_ml[["svm"]][[j]] <- run_svm(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params, 
        indcv     = indcv, 
        iter      = j, 
        plim      = plim
      )
    }
    
    
    if("bartc" %in% algorithms){
      fit_ml[["bartc"]][[j]] <- run_bartc(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params, 
        indcv     = indcv, 
        iter      = j, 
        plim      = plim
      )
    }

    if("bart" %in% algorithms){
      fit_ml[["bart"]][[j]] <- run_bartmachine(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params, 
        indcv     = indcv, 
        iter      = j, 
        plim      = plim
      )
    }    
    
    if("boost" %in% algorithms){
      fit_ml[["boost"]][[j]] <- run_boost(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params, 
        indcv     = indcv, 
        iter      = j, 
        plim      = plim
      )
    }
    
    if("random_forest" %in% algorithms){
      fit_ml[["random_forest"]][[j]] <- run_random_forest(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params, 
        indcv     = indcv, 
        iter      = j, 
        plim      = plim
      )
    }
     
    if("bagging" %in% algorithms){
      fit_ml[["bagging"]][[j]] <- run_bagging(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params, 
        indcv     = indcv, 
        iter      = j, 
        plim      = plim
      )
    }

    if("cart" %in% algorithms){
      fit_ml[["cart"]][[j]] <- run_cart(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params, 
        indcv     = indcv, 
        iter      = j, 
        plim      = plim
      )
    }    

  } ## end of fold 

  return(list(
    params = params, fit_ml = fit_ml, 
    Ycv = Ycv, Tcv = Tcv, indcv = indcv, plim = plim
  ))
}

utils::globalVariables(c("Treat","aupec", "sd", "Pval", "aupec.y", "fraction", "AUPECmin", "AUPECmax", ".", "fit", "out", "pape", "alg", "papep", "papd", "type"))
