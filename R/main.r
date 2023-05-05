#' Estimate individual treatment rules (ITR)
#' @param treatment Treatment variable
#' @param form a formula object that takes the form \code{y ~ T + x1 + x2 + ...}. 
#' @param data
#'   A data frame that contains the outcome \code{y} and the treatment \code{T}. 
#' @param algorithms
#'   List of machine learning algorithms to be used. 
#' @param budget
#'   Proportion of treated units.
#' @param n_folds
#'   Number of cross-validation folds. Default is 5.
#' @param split_ratio
#'   Split ratio between train and test set under sample splitting. Default is 0.
#' @param ngates
#'   The number of groups to separate the data into. The groups are determined by tau. Default is 5.
#' @param preProcess caret parameter
#' @param weights caret parameter
#' @param trControl caret parameter
#' @param tuneGrid caret parameter
#' @param tuneLength caret parameter
#' @param ... Additional arguments passed to \code{caret::train}
#' @import dplyr
#' @import rlearner
#' @importFrom rlang !! sym
#' @export
#' @return An object of \code{itr} class
estimate_itr <- function(
    treatment,
    form,
    data,
    algorithms,
    budget,
    n_folds = 5,
    split_ratio = 0,
    ngates = 5,
    preProcess = NULL,
    weights = NULL,
    trControl = caret::trainControl(method = "none"),
    tuneGrid = NULL,
    tuneLength = ifelse(trControl$method == "none", 1, 3),
    ...
) {

  # specify the outcome and covariates
  convert_data <- convert_formula(form, data, treatment)

  outcome <- convert_data$outcome
  covariates <- convert_data$covariates
  data <- convert_data$data

  ## caret parameters
  metric = ifelse(is.factor(data[outcome]), "Accuracy", "RMSE")
  maximize = ifelse(metric %in% c("RMSE", "logLoss", "MAE", "logLoss"), FALSE, TRUE)

  caret_algorithms <- names(caret::getModelInfo())

  ## rlearn algorithms
  rlearner_algorithms <- c("rkern", "rlasso","rboost", "tkern", "tlasso", "tboost", "skern", "slasso", "sboost", "xkern", "xlasso", "xboost", "ukern", "ulasso", "uboost")

  # caret train parameters
  train_params <- list(
    # train_method = train_method,
    preProcess = preProcess,
    weights = weights,
    metric = metric,
    maximize = maximize,
    trControl = trControl,
    tuneGrid = tuneGrid,
    tuneLength = tuneLength
  )

  ## number of algorithms
  n_alg <- length(algorithms)

  ## some working variables
  n_df <- nrow(data)
  n_X  <- length(data) - 1
  n_folds <- n_folds
  cv <- ifelse(split_ratio > 0, FALSE, TRUE)

  params <- list(
    n_df = n_df, n_folds = n_folds, n_alg = n_alg, split_ratio = split_ratio, ngates = ngates, cv = cv, 
    train_params = train_params, caret_algorithms = caret_algorithms, rlearner_algorithms = rlearner_algorithms)

  df <- list(algorithms = algorithms, outcome = outcome, data = data, treatment = treatment)

  ## loop over all outcomes
  estimates <- vector("list", length = length(outcome))
  
  ## data to use
  ## rename outcome and treatment variable
  data_filtered <- data %>%
    select(Y = !!sym(outcome), Treat = !!sym(treatment), all_of(covariates))

  ## cross-validation
  if(cv == TRUE){
    ## create folds
    treatment_vec <- data_filtered %>% dplyr::pull(Treat)
    folds <- caret::createFolds(treatment_vec, k = n_folds)
  }

  ## sample splitting
  if(cv == FALSE){
    folds = n_folds
  }

  ## run
  estimates <- itr_single_outcome(
    data       = data_filtered,
    algorithms = algorithms,
    params     = params,
    folds      = folds,
    budget     = budget,
    ...
  )

  out <- list(estimates = estimates, df = df)

  class(out) <- c("itr", class(out))

  return(out)

}
#' Evaluate ITR for Single Outcome
#'
#' @importFrom purrr map
#' @importFrom dplyr pull
#' @param data A dataset.
#' @param algorithms Machine learning algorithms.
#' @param params A list of parameters.
#' @param folds Number of folds.
#' @param budget The maximum percentage of population that can be treated under the budget constraint.
#' @param ... Additional arguments passed to \code{caret::train}
#' @return A list of estimates.

itr_single_outcome <- function(
    data,
    algorithms,
    params,
    folds,
    budget,
    ...
) {

  ## obj to store outputs
  fit_ml <- lapply(1:params$n_alg, function(x) vector("list", length = params$n_folds))
  names(fit_ml) <- algorithms

  models <- lapply(1:params$n_alg, function(x) vector("list", length = params$n_folds))
  names(models) <- algorithms

  ## caret parameters
  caret_algorithms = params$caret_algorithms

  ## rlearn algorithms
  rlearner_algorithms = params$rlearner_algorithms

## =================================
## sample splitting
## =================================

  if(params$cv == FALSE){

    cat('Evaluate ITR under sample splitting ...\n')

    ## ---------------------------------
    ## data split
    ## ---------------------------------

    # create split series of test/training partitions
    split <- caret::createDataPartition(data$Treat,
                                        p = params$split_ratio,
                                        list = FALSE)
    trainset = data[split,]
    testset = data[-split,]


    Tcv <- dplyr::pull(testset, "Treat")
    Ycv <- dplyr::pull(testset, "Y")
    indcv <- rep(0, length(Ycv))

    params$n_tb <- max(table(indcv))

    ## ---------------------------------
    ## run ML
    ## ---------------------------------

    # prepare data
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

    # loop over all algorithms
    for (i in seq_along(algorithms)) {

      # check if algorithm is in the caret package
      if (algorithms[i] %in% caret_algorithms) {
        
        # set the train_method to the algorithm
        train_method = algorithms[i]

        # run the algorithm
        caret_est <- run_caret(
          dat_train = training_data_elements,
          dat_test  = testing_data_elements,
          dat_total = total_data_elements,
          params    = params,
          budget    = budget,
          indcv     = 1,
          iter      = 1,
          train_method = train_method,
          ...
        )

        # store the results
        fit_ml[[algorithms[i]]] <- caret_est$test
        models[[algorithms[i]]] <- caret_est$train

      }

      # check if algorithm is in the rlearner package
      if (algorithms[i] %in% rlearner_algorithms) {
        
        # set the train_method to the algorithm
        train_method = algorithms[i]

        # run the algorithm
        rlearner_est <- run_rlearner(
          dat_train = training_data_elements,
          dat_test  = testing_data_elements,
          dat_total = total_data_elements,
          params    = params,
          budget    = budget,
          indcv     = 1,
          iter      = 1,
          train_method = train_method)

        # store the results
        fit_ml[[algorithms[i]]] <- rlearner_est$test
        models[[algorithms[i]]] <- rlearner_est$train

      }
    
    }

    if ("causal_forest" %in% algorithms) {
      # run causal forest
      est <- run_causal_forest(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        budget    = budget,
        indcv     = 1, #indcv and iter set to 1 for sample splitting
        iter      = 1
      )
      # store the results
      fit_ml[["causal_forest"]] <- est$test
      models[["causal_forest"]] <- est$train
    }

    if("lasso" %in% algorithms){
      # run lasso
      est <- run_lasso(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
      # store the results
      fit_ml[["lasso"]] <- est$test
      models[["lasso"]] <- est$train
    }

    # if("rlasso" %in% algorithms){
    #   # run lasso
    #   est <- run_rlasso(
    #     dat_train = training_data_elements,
    #     dat_test  = testing_data_elements,
    #     dat_total = total_data_elements,
    #     params    = params,
    #     indcv     = 1,
    #     iter      = 1,
    #     budget    = budget
    #   )
    #   # store the results
    #   fit_ml[["rlasso"]] <- est$test
    #   models[["rlasso"]] <- est$train
    # }

    # if("rboost" %in% algorithms){
    #   # run rboost
    #   est <- run_rboost(
    #     dat_train = training_data_elements,
    #     dat_test  = testing_data_elements,
    #     dat_total = total_data_elements,
    #     params    = params,
    #     indcv     = 1,
    #     iter      = 1,
    #     budget    = budget
    #   )
    #   # store the results
    #   fit_ml[["rboost"]] <- est$test
    #   models[["rboost"]] <- est$train
    # }

    # if("rkern" %in% algorithms){
    #   # run rkern
    #   est <- run_rboost(
    #     dat_train = training_data_elements,
    #     dat_test  = testing_data_elements,
    #     dat_total = total_data_elements,
    #     params    = params,
    #     indcv     = 1,
    #     iter      = 1,
    #     budget    = budget
    #   )
    #   # store the results
    #   fit_ml[["rkern"]] <- est$test
    #   models[["rkern"]] <- est$train
    # }

    # if("svm" %in% algorithms){
    #   # run svm
    #   est <- run_svm(
    #     dat_train = training_data_elements,
    #     dat_test  = testing_data_elements,
    #     dat_total = total_data_elements,
    #     params    = params,
    #     indcv     = 1,
    #     iter      = 1,
    #     budget    = budget
    #   )
    #   # store the results
    #   fit_ml[["svm"]] <- est$test
    #   models[["svm"]] <- est$train
    # }


    if("bartc" %in% algorithms){
      # run bartcause
      est <- run_bartc(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
      # store the results
      fit_ml[["bartc"]] <- est$test
      models[["bartc"]] <- est$train
    }

    # if("bart" %in% algorithms){
    #   # run bart
    #   est <- run_bartmachine(
    #     dat_train = training_data_elements,
    #     dat_test  = testing_data_elements,
    #     dat_total = total_data_elements,
    #     params    = params,
    #     indcv     = 1,
    #     iter      = 1,
    #     budget    = budget
    #   )
    #   # store the results
    #   fit_ml[["bart"]] <- est$test
    #   models[["bart"]] <- est$train
    # }

    # if("boost" %in% algorithms){
    #   # run boost
    #   est <- run_boost(
    #     dat_train = training_data_elements,
    #     dat_test  = testing_data_elements,
    #     dat_total = total_data_elements,
    #     params    = params,
    #     indcv     = 1,
    #     iter      = 1,
    #     budget    = budget
    #   )
    #   # store the results
    #   fit_ml[["boost"]] <- est$test
    #   models[["boost"]] <- est$train
    # }

    # if("random_forest" %in% algorithms){
    #   # run random forest
    #   est <- run_random_forest(
    #     dat_train = training_data_elements,
    #     dat_test  = testing_data_elements,
    #     dat_total = total_data_elements,
    #     params    = params,
    #     indcv     = 1,
    #     iter      = 1,
    #     budget    = budget
    #   )
    #   # store the results
    #   fit_ml[["random_forest"]] <- est$test
    #   models[["random_forest"]] <- est$train
    # }

    if("bagging" %in% algorithms){
      # run bagging
      est <- run_bagging(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
      # store the results
      fit_ml[["bagging"]] <- est$test
      models[["bagging"]] <- est$train
    }

    if("cart" %in% algorithms){
      # run cart
      est <- run_cart(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
      # store the results
      fit_ml[["cart"]] <- est$test
      models[["cart"]] <- est$train
    }

  }

## =================================
## k-folds cross-validation
## =================================

  if(params$cv == TRUE) {

    cat('Evaluate ITR with cross-validation ...\n')

    Tcv <- dplyr::pull(data, "Treat")
    Ycv <- dplyr::pull(data, "Y")
    indcv <- rep(0, length(Ycv))

    params$n_tb <- max(table(indcv))


    ## loop over j number of folds

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
      
      # loop over all algorithms
      for (i in seq_along(algorithms)) {

        # check if algorithm is in the caret package
        if (algorithms[i] %in% caret_algorithms) {
          
          # set the train_method to the algorithm
          train_method = algorithms[i]

          # run the algorithm
          caret_est <- run_caret(
            dat_train     = training_data_elements,
            dat_test      = testing_data_elements,
            dat_total     = total_data_elements,
            train_method  = train_method,
            params        = params,
            indcv         = indcv,
            iter          = j,
            budget        = budget,
            ...
          )

          # store the results
          fit_ml[[algorithms[i]]][[j]] <- caret_est$test
          models[[algorithms[i]]][[j]] <- caret_est$train

        }

        # check if algorithm is in the rlearner package
        if (algorithms[i] %in% rlearner_algorithms) {

          # set the train_method to the algorithm
          train_method = algorithms[i]

          # run the algorithm
          rlearner_est <- run_rlearner(
            dat_train     = training_data_elements,
            dat_test      = testing_data_elements,
            dat_total     = total_data_elements,
            train_method  = train_method,
            params        = params,
            indcv         = indcv,
            iter          = j,
            budget        = budget,
            ...
          )

          # store the results
          fit_ml[[algorithms[i]]][[j]] <- rlearner_est$test
          models[[algorithms[i]]][[j]] <- rlearner_est$train

        }
      }

      if ("causal_forest" %in% algorithms) {
        # run causal forest
        est <- run_causal_forest(
          dat_train = training_data_elements,
          dat_test  = testing_data_elements,
          dat_total = total_data_elements,
          params    = params,
          indcv     = indcv,
          iter      = j,
          budget    = budget
        )
        # store the results
        fit_ml[["causal_forest"]][[j]] <- est$test
        models[["causal_forest"]][[j]] <- est$train
      }

      if("lasso" %in% algorithms){
        # run lasso
        est <- run_lasso(
          dat_train = training_data_elements,
          dat_test  = testing_data_elements,
          dat_total = total_data_elements,
          params    = params,
          indcv     = indcv,
          iter      = j,
          budget    = budget
        )
        # store the results
        fit_ml[["lasso"]][[j]] <- est$test
        models[["lasso"]][[j]] <- est$train
      }

      # if("rlasso" %in% algorithms){
      #   # run lasso
      #   est <- run_rlasso(
      #     dat_train = training_data_elements,
      #     dat_test  = testing_data_elements,
      #     dat_total = total_data_elements,
      #     params    = params,
      #     indcv     = indcv,
      #     iter      = j,
      #     budget    = budget
      #   )
      #   # store the results
      #   fit_ml[["rlasso"]][[j]] <- est$test
      #   models[["rlasso"]][[j]] <- est$train
      # }

      # if("rboost" %in% algorithms){
      #   # run lasso
      #   est <- run_rboost(
      #     dat_train = training_data_elements,
      #     dat_test  = testing_data_elements,
      #     dat_total = total_data_elements,
      #     params    = params,
      #     indcv     = indcv,
      #     iter      = j,
      #     budget    = budget
      #   )
      #   # store the results
      #   fit_ml[["rboost"]][[j]] <- est$test
      #   models[["rboost"]][[j]] <- est$train
      # }

      # if("rkern" %in% algorithms){
      #   # run rkern
      #   est <- run_rkern(
      #     dat_train = training_data_elements,
      #     dat_test  = testing_data_elements,
      #     dat_total = total_data_elements,
      #     params    = params,
      #     indcv     = indcv,
      #     iter      = j,
      #     budget    = budget
      #   )
      #   # store the results
      #   fit_ml[["rkern"]][[j]] <- est$test
      #   models[["rkern"]][[j]] <- est$train
      # }

      # if("svm" %in% algorithms){
      #   # run svm
      #   est <- run_svm(
      #     dat_train = training_data_elements,
      #     dat_test  = testing_data_elements,
      #     dat_total = total_data_elements,
      #     params    = params,
      #     indcv     = indcv,
      #     iter      = j,
      #     budget    = budget
      #   )
      #   # store the results
      #   fit_ml[["svm"]][[j]] <- est$test
      #   models[["svm"]][[j]] <- est$train
      # }


      if("bartc" %in% algorithms){
        # run bartcause
        est <- run_bartc(
          dat_train = training_data_elements,
          dat_test  = testing_data_elements,
          dat_total = total_data_elements,
          params    = params,
          indcv     = indcv,
          iter      = j,
          budget    = budget
        )
        # store the results
        fit_ml[["bartc"]][[j]] <- est$test
        models[["bartc"]][[j]] <- est$train
      }

      # if("bart" %in% algorithms){
      #   # run bart
      #   est <- run_bartmachine(
      #     dat_train = training_data_elements,
      #     dat_test  = testing_data_elements,
      #     dat_total = total_data_elements,
      #     params    = params,
      #     indcv     = indcv,
      #     iter      = j,
      #     budget    = budget
      #   )
      #   # store the results
      #   fit_ml[["bart"]][[j]] <- est$test
      #   models[["bart"]][[j]] <- est$train
      # }

      # if("boost" %in% algorithms){
      #   # run boost
      #   est <- run_boost(
      #     dat_train = training_data_elements,
      #     dat_test  = testing_data_elements,
      #     dat_total = total_data_elements,
      #     params    = params,
      #     indcv     = indcv,
      #     iter      = j,
      #     budget    = budget
      #   )
      #   # store the results
      #   fit_ml[["boost"]][[j]] <- est$test
      #   models[["boost"]][[j]] <- est$train
      # }

      # if("random_forest" %in% algorithms){
      #   # run random forest
      #   est <- run_random_forest(
      #     dat_train = training_data_elements,
      #     dat_test  = testing_data_elements,
      #     dat_total = total_data_elements,
      #     params    = params,
      #     indcv     = indcv,
      #     iter      = j,
      #     budget    = budget
      #   )
      #   # store the results
      #   fit_ml[["random_forest"]][[j]] <- est$test
      #   models[["random_forest"]][[j]] <- est$train
      # }

      if("bagging" %in% algorithms){
        # run bagging
        est <- run_bagging(
          dat_train = training_data_elements,
          dat_test  = testing_data_elements,
          dat_total = total_data_elements,
          params    = params,
          indcv     = indcv,
          iter      = j,
          budget    = budget
        )
        # store the results
        fit_ml[["bagging"]][[j]] <- est$test
        models[["bagging"]][[j]] <- est$train
      }

      if("cart" %in% algorithms){
        # run cart
        est <- run_cart(
          dat_train = training_data_elements,
          dat_test  = testing_data_elements,
          dat_total = total_data_elements,
          params    = params,
          indcv     = indcv,
          iter      = j,
          budget    = budget
        )
        # store the results
        fit_ml[["cart"]][[j]] <- est$test
        models[["cart"]][[j]] <- est$train
      }
    } ## end of fold
  }

  return(list(
    params = params, fit_ml = fit_ml, models = models,
    Ycv = Ycv, Tcv = Tcv, indcv = indcv, budget = budget
  ))
}
#' Evaluate ITR
#' @param fit Fitted model. Usually an output from \code{estimate_itr}
#' @param ... Further arguments passed to the function.
#' @return An object of \code{itr} class
#' @export
evaluate_itr <- function(fit, ...){

  estimates  <- fit$estimates
  cv         <- estimates$params$cv
  df         <- fit$df
  algorithms <- fit$df$algorithms
  outcome    <- fit$df$outcome

  ## compute qoi
  qoi      <- vector("list", length = length(outcome))
  qoi <- compute_qoi(estimates, algorithms)

  out <- list(
    qoi = qoi, cv = cv, df = df, estimates = estimates)

  class(out) <- c("itr", class(out))

  return(out)

}

#' Conduct hypothesis tests
#' @param fit Fitted model. Usually an output from \code{estimate_itr}
#' @param nsim Number of Monte Carlo simulations used to simulate the null distributions. Default is 1000.
#' @param ... Further arguments passed to the function.
#' @return An object of \code{itr} class
#' @export
test_itr <- function(
    fit,
    nsim = 1000,
    ...
) {

  # test parameters
  estimates  <- fit$estimates
  cv         <- estimates$params$cv
  fit_ml     <- estimates$fit_ml
  Tcv        <- estimates$Tcv
  Ycv        <- estimates$Ycv
  indcv      <- estimates$indcv
  n_folds    <- estimates$params$n_folds
  ngates     <- estimates$params$ngates
  algorithms <- fit$df$algorithms
  outcome    <- fit$df$outcome
  # run tests

  ## =================================
  ## sample splitting
  ## =================================

  if(cv == FALSE){
    cat('Conduct hypothesis tests for GATEs unde sample splitting ...\n')

    ## create empty lists to for consistcv and hetcv
    consist <- list()
    het <- list()

      # model with a single outcome
      ## run consistency tests for each model
      if ("causal_forest" %in% algorithms) {
        consist[["causal_forest"]] <- consist.test(
          T   = Tcv,
          tau = fit_ml$causal_forest$tau,
          Y   = Ycv,
          ngates = ngates)

        het[["causal_forest"]] <- het.test(
          T   = Tcv,
          tau = fit_ml$causal_forest$tau,
          Y   = Ycv,
          ngates = ngates)
      }

      if ("lasso" %in% algorithms) {
        consist[["lasso"]] <- consist.test(
          T   = Tcv,
          tau = fit_ml$lasso$tau,
          Y   = Ycv,
          ngates = ngates)

        het[["lasso"]] <- het.test(
          T   = Tcv,
          tau = fit_ml$lasso$tau,
          Y   = Ycv,
          ngates = ngates)
      }

  }

  ## =================================
  ## cross validation
  ## =================================

  if(cv == TRUE){
    cat('Conduct hypothesis tests for GATEs unde cross-validation ...\n')

      ## create empty lists to for consistcv and hetcv
    consistcv <- list()
    hetcv <- list()

      ## run consistency tests for each model

      if ("causal_forest" %in% algorithms) {
        consistcv[["causal_forest"]] <- consistcv.test(
          T   = Tcv,
          tau = gettaucv(fit),
          Y   = Ycv,
          ind = indcv,
          ngates = ngates)
      }

      ## run heterogeneity tests for each model
      if ("causal_forest" %in% algorithms) {
        hetcv[["causal_forest"]] <- hetcv.test(
          T   = Tcv,
          tau = gettaucv(fit), # a matrix of length(total sample size) x n_folds
          Y   = Ycv,
          ind = indcv,
          ngates = ngates)
      }

  }

  # formulate and return output
  if(cv == FALSE){
    tests <- list(consist = consist,
                  het = het)
    return(tests)
  }
  if(cv == TRUE){
    tests_cv <- list(consistcv = consistcv,
                    hetcv = hetcv)
    return(tests_cv)
  }

}


utils::globalVariables(c("Treat", "aupec", "sd", "Pval", "aupec.y", "fraction", "AUPECmin", "AUPECmax", ".", "fit", "out", "pape", "alg", "papep", "papd", "type", "gate", "group", "qnorm", "vec", "Y"))

