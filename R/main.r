##
##
##

# setwd("/n/home04/jialuli/fasrc/data/sys/dashboard/itr")

source("itr_helpers.R")
source("itr_qoi.R")


## read in ml algorithms
file.sources = list.files(pattern="itr_run*")
sapply(file.sources,source,.GlobalEnv)

## relative location
here::i_am("itr_qoi.R")


# source("itr_run_causal-forest.R")
# source("itr_run_svm.R")
# source("itr_run_lasso.R")
# source("itr_run_bart.R")
# source("itr_run_boost.R")
# source("itr_run_random-forest.R")
# source("itr_run_bagging.R")
# source("itr_run_cart.R")



#' Evaluate ITR
#' 
#' @param var_outcome 
#' @param var_treatment
#' @param data 
#'   A data frame that contains \code{var_outcome} and \code{var_treatment}.
#' @param algorithms 
#'   Machine learning algorithms.  
#' @param plim 
#'   Proportion of treated units.
#' @param n_folds 
#'   Number of cross-validation folds.
#' @param options  
#'   List of options.
#'   
#' @importFrom dplyr %>% select
#' @importFrom rlang !! sym
#' 
run_itr <- function(
  var_outcome,
  var_treatment, 
  var_covariates,
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
  estimates <- qoi <- vector("list", length = length(var_outcome))
  for (m in 1:length(var_outcome)) {

    ## data to use 
    ## rename outcome and treatment variable 
    data_filtered <- data %>% 
      select(Y = !!sym(var_outcome[m]), Treat = !!sym(var_treatment), all_of(var_covariates))
    
    ## create folds 
    treatment_vec <- data_filtered %>% pull(Treat)
    folds <- createFolds(treatment_vec, k = NFOLDS)


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
#' @importFrom dpylr pull
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
      outcome_var = "Y", treatment_var = "Treat", data = trainset
    )

    testing_data_elements <- create_ml_arguments(
      outcome_var = "Y", treatment_var = "Treat", data = testset
    )
    
    total_data_elements <- create_ml_arguments(
      outcome_var = "Y", treatment_var = "Treat", data = data
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



