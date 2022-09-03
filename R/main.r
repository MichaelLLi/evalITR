#' Evaluate ITR
#' 
#' @param outcome XXX
#' @param treatment YYY
#' @param data 
#'   A data frame that contains \code{outcome} and \code{treatment}.
#' @param algorithms 
#'   Machine learning algorithms.  
#' @param plim 
#'   Proportion of treated units.
#' @param n_folds 
#'   Number of cross-validation folds.
#' @param options  
#'   List of options.
#'  
#' @import dplyr
#' @importFrom rlang !! sym
#' @export
run_itr <- function(
  outcome,
  treatment, 
  covariates,
  data, 
  algorithms,
  plim,
  n_folds,
  plot = FALSE
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
      plim       = plim,
      plot       = plot
    )

    
    ## format output 
    qoi[[m]] <- compute_qoi(estimates[[m]], algorithms)
    
  }


  return(list(qoi       = qoi,
              estimates = estimates))

}


#' Evaluate ITR for Single Outcome
#' 
#' @importFrom purrr map
#' @importFrom dplyr pull
itr_single_outcome <- function(
  data, 
  algorithms,
  params, 
  folds,
  plim,
  plot
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
        plim      = plim,
        plot      = plot 
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
        plim      = plim,
        plot      = plot          
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
        plim      = plim,
        plot      = plot 
      )
    }
    
    
    if("bart" %in% algorithms){
      fit_ml[["bart"]][[j]] <- run_bart(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params, 
        indcv     = indcv, 
        iter      = j, 
        plim      = plim,
        plot      = plot 
      )
    }

    if("bartmachine" %in% algorithms){
      fit_ml[["bartmachine"]][[j]] <- run_bartmachine(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params, 
        indcv     = indcv, 
        iter      = j, 
        plim      = plim,
        plot      = plot          
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
        plim      = plim,
        plot      = plot  
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
        plim      = plim,
        plot      = plot  
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
        plim      = plim,
        plot      = plot  
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
        plim      = plim,
        plot      = plot  
      )
    }
    
    if("lm" %in% algorithms){
      fit_ml[["lm"]][[j]] <- run_lm(
        dat_train = training_data_elements
      )
    }
    

  } ## end of fold 

  return(list(
    params = params, fit_ml = fit_ml, 
    Ycv = Ycv, Tcv = Tcv, indcv = indcv, plim = plim
  ))
}



