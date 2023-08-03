#' Estimate individual treatment rules (ITR)
#' @param treatment Treatment variable
#' @param form a formula object that takes the form \code{y ~ T + x1 + x2 + ...}.
#' @param data
#'   A data frame that contains the outcome \code{y} and the treatment \code{T}.
#' @param algorithms
#'   List of machine learning algorithms to be used.
#' @param budget The maximum percentage of population that can be treated under the budget constraint.
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
#' @param user_model A user-defined function to create an ITR. The function should take the data as input and return a model to estimate the ITR.
#' @param SL_library A list of machine learning algorithms to be used in the super learner.
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
    user_model = NULL,
    SL_library = NULL,
    ...
) {

  # specify the outcome and covariates
  convert_data <- convert_formula(as.formula(form), data, treatment)

  outcome <- convert_data$outcome
  covariates <- convert_data$covariates
  data <- convert_data$data

  # caret parameters
  metric = ifelse(is.factor(data[outcome]), "Accuracy", "RMSE")
  maximize = ifelse(metric %in% c("RMSE", "logLoss", "MAE", "logLoss"), FALSE, TRUE)

  caret_algorithms <- names(caret::getModelInfo())

  # rlearn algorithms
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

  # combine package algs with user's own function
  algorithms <- c(algorithms, user_model)

  # some working variables
  n_alg <- length(algorithms)
  n_df <- nrow(data)
  n_X  <- length(data) - 1
  n_folds <- n_folds
  cv <- ifelse(split_ratio > 0, FALSE, TRUE)

  params <- list(
    n_df = n_df, n_folds = n_folds, n_alg = n_alg, split_ratio = split_ratio, ngates = ngates, cv = cv,
    train_params = train_params, caret_algorithms = caret_algorithms, rlearner_algorithms = rlearner_algorithms, SL_library = SL_library)

  df <- list(algorithms = algorithms, outcome = outcome, data = data, treatment = treatment)

  # loop over all outcomes
  estimates <- vector("list", length = length(outcome))

  # data to use
  data_filtered <- data %>%
    select(Y = !!sym(outcome), T = !!sym(treatment), all_of(covariates))

  # cross-validation
  if(cv == TRUE){
    # create folds
    treatment_vec <- data_filtered %>% dplyr::pull(T)
    folds <- caret::createFolds(treatment_vec, k = n_folds)
  }

  # sample splitting
  if(cv == FALSE){
    folds = n_folds
  }

  # run
  estimates <- fit_itr(
    data       = data_filtered,
    algorithms = algorithms,
    params     = params,
    folds      = folds,
    budget     = budget,
    user_model = user_model,
    ...
  )

  out <- list(estimates = estimates, df = df)

  class(out) <- c("itr", class(out))

  return(out)

}
#' Estimate ITR for Single Outcome
#'
#' @importFrom purrr map
#' @importFrom dplyr pull
#' @param data A dataset.
#' @param algorithms Machine learning algorithms.
#' @param params A list of parameters.
#' @param folds Number of folds.
#' @param budget The maximum percentage of population that can be treated under the budget constraint.
#' @param user_model User's own function to estimated the ITR.
#' @param ... Additional arguments passed to \code{caret::train}
#' @return A list of estimates.

fit_itr <- function(
    data,
    algorithms,
    params,
    folds,
    budget,
    user_model,
    ...
) {

  # obj to store outputs
  fit_ml <- lapply(1:params$n_alg, function(x) vector("list", length = params$n_folds))
  names(fit_ml) <- algorithms

  models <- lapply(1:params$n_alg, function(x) vector("list", length = params$n_folds))
  names(models) <- algorithms

  # caret parameters
  caret_algorithms = params$caret_algorithms

  # rlearn algorithms
  rlearner_algorithms = params$rlearner_algorithms

  # super learner library
  SL_library = params$SL_library

## =================================
## sample splitting
## =================================

  if(params$cv == FALSE){

    cat('Evaluate ITR under sample splitting ...\n')

    ## ---------------------------------
    ## data split
    ## ---------------------------------

    # create split series of test/training partitions
    split <- caret::createDataPartition(
      data$T,
      p = params$split_ratio,
      list = FALSE)

    trainset = data[split,]
    testset = data[-split,]


    Tcv <- dplyr::pull(testset, "T")
    Ycv <- dplyr::pull(testset, "Y")
    indcv <- rep(0, length(Ycv))

    params$n_tb <- max(table(indcv))

    ## ---------------------------------
    ## run ML
    ## ---------------------------------

    # prepare data
    training_data_elements <- create_ml_arguments(
      outcome = "Y", treatment = "T", data = trainset
    )

    testing_data_elements <- create_ml_arguments(
      outcome = "Y", treatment = "T", data = testset
    )

    total_data_elements <- create_ml_arguments(
      outcome = "Y", treatment = "T", data = data
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

    # user defined algorithm
    if (!is.null(user_model)){

    # run the algorithm
    user_est <- run_user(
      dat_train = trainset,
      dat_test  = testset,
      dat_total = data,
      params    = params,
      budget    = budget,
      indcv     = 1,
      iter      = 1,
      train_method = user_model,
      ...
      )

    # store the results
    model_names = as.character(substitute(user_model))
    fit_ml[[model_names]] <- user_est$test
    models[[model_names]] <- user_est$train

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

    if("SuperLearner" %in% algorithms){
      # run SuperLearner
      est <- run_superLearner(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget,
        train_method = "SuperLearner",
        SL_library    = SL_library,
        ...
      )
      # store the results
      fit_ml[["SuperLearner"]] <- est$test
      models[["SuperLearner"]] <- est$train
    }


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

    Tcv <- dplyr::pull(data, "T")
    Ycv <- dplyr::pull(data, "Y")
    indcv <- rep(0, length(Ycv))

    params$n_tb <- max(table(indcv))


    # loop over j number of folds

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

      # prepare data
      training_data_elements <- create_ml_arguments(
        outcome = "Y", treatment = "T", data = trainset
      )

      testing_data_elements <- create_ml_arguments(
        outcome = "Y", treatment = "T", data = testset
      )

      total_data_elements <- create_ml_arguments(
        outcome = "Y", treatment = "T", data = data
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

      # user defined algorithm
      if (!is.null(user_model)){

      # run the algorithm
      user_est <- run_user(
        dat_train = trainset,
        dat_test  = testset,
        dat_total = data,
        params    = params,
        budget    = budget,
        indcv     = indcv,
        iter      = j,
        train_method = user_model,
        ...
      )

      # store the results
      model_names <- as.character(substitute(user_model))

      fit_ml[[model_names]][[j]] <- user_est$test
      models[[model_names]][[j]] <- user_est$train

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

      if("SuperLearner" %in% algorithms){
        # run SuperLearner
        est <- run_superLearner(
          dat_train = training_data_elements,
          dat_test  = testing_data_elements,
          dat_total = total_data_elements,
          params    = params,
          indcv     = indcv,
          iter      = j,
          budget    = budget,
          train_method = "SuperLearner",
          SL_library    = SL_library,
          ...
        )
        # store the results
        fit_ml[["SuperLearner"]][[j]] <- est$test
        models[["SuperLearner"]][[j]] <- est$train
      }

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
    } # end of fold
  }

  return(list(
    params = params, fit_ml = fit_ml, models = models,
    Ycv = Ycv, Tcv = Tcv, indcv = indcv, budget = budget
  ))
}


#' Evaluate ITR
#' @param fit Fitted model. Usually an output from \code{estimate_itr}
#' @param user_itr  A user-defined function to create an ITR. The function should take the data as input and return an unit-level continuous score for treatment assignment. We assume those that have score less than 0 should not have treatment. The default is \code{NULL}, which means the ITR will be estimated from the \code{estimate_itr}.
#' @param outcome A character string of the outcome variable name.
#' @param treatment A character string of the treatment variable name.
#' @param data A data frame containing the variables specified in \code{outcome}, \code{treatment}, and \code{tau}.
#' @param budget The maximum percentage of population that can be treated under the budget constraint.
#' @param ngates The number of gates to use for the ITR. The default is 5.
#' A user-defined function to create an ITR. The function should take the data as input and return an ITR. The output is a vector of the unit-level binary treatment that would have been assigned by the individualized treatment rule. The default is \code{NULL}, which means the ITR will be estimated from the \code{estimate_itr}.
#' See \code{?evaluate_itr} for an example.
#' @param ... Further arguments passed to the function.
#' @return An object of \code{itr} class
#' @export
evaluate_itr <- function(
  fit = NULL,
  user_itr = NULL,
  outcome = c(),
  treatment = c(),
  data = list(),
  budget = 1,
  ngates = 5,
  ...){

  # parameters
  out_algs <- list()
  out_user  <- list()

  # estimate ITR from ML algorithms
  if(!is.null(fit)){

    # estimate ITR from the fitted model
    estimates  <- fit$estimates
    cv         <- estimates$params$cv
    df         <- fit$df
    algorithms <- fit$df$algorithms
    outcome    <- fit$df$outcome

    # compute qoi
    qoi <- vector("list", length = length(outcome))
    qoi <- compute_qoi(estimates, algorithms)

    # store the results
    out_algs <- list(
      qoi = qoi,
      cv = cv,
      df = df,
      estimates = estimates)
  }

  # get ITR from the user-defined function
  if(!is.null(user_itr)){
    df  = data
    Ycv = data[, outcome]
    Tcv = data[, treatment]

    estimates <- list(
      Ycv = Ycv,
      Tcv = Tcv,
      algorithms = "user-defined")
    cv   <- FALSE

    # compute qoi
    qoi   <- vector("list", length = length(outcome))
    qoi   <- compute_qoi_user(
      user_itr, Tcv, Ycv, data, ngates, budget, ...)

    # store the results
    out_user <- list(
      qoi = qoi,
      cv = cv,
      df = df,
      estimates = estimates)
  }

  # store the results
  out <- list(
    out_algs = out_algs,
    out_user = out_user)

  class(out) <- c("itr", class(out))

  return(out)

}

#' Conduct Nonparametric Statistical Tests with fitted ITRs
#'
#' This function takes estimated ITRs from \code{estimate_itr}'s output to conduct nonparametric statistical tests of 1) treatment effect heterogeneity, and of 2) rank-consistent treatment effect heterogeneity.
#' The details of the design of the two tests are given in Imai and Li (2022).
#'
#' @param fit Estimated ITRs in a fitted model. Usually an output from \code{estimate_itr}.
#' @param nsim Number of Monte Carlo simulations used to simulate the null distributions. Default is 1000.
#' @param ... Further arguments passed to the function.
#' @return A list of \code{test_itr} class that contains the following items: \item{dataframe.het}{The estimated
#' p-value of the null hypothesis, and the test statistic for the test of group-level heterogeneity for each fitted machine learning models.} \item{dataframe.consist}{The estimated
#' p-value of the null hypothesis, and the test statistic for the test of rank consistency for each fitted machine learning models.}
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

  # caret and rlearner parameters
  caret_algorithms <- estimates$params$caret_algorithms
  rlearner_algorithms <- estimates$params$rlearner_algorithms

  # super learner library
  SL_library <- estimates$params$SL_library

  # run tests

  ## =================================
  ## sample splitting
  ## =================================

  if(cv == FALSE){
    cat('Conduct hypothesis tests for GATEs unde sample splitting ...\n')

    # create empty lists to for consistcv and hetcv
    consist <- list()
    het <- list()

    # run consistency and heterogeneity tests for each model
    for (i in algorithms) {

      consist[[i]] <- consist.test(
        T   = Tcv,
        tau = fit_ml[[i]]$tau,
        Y   = Ycv,
        ngates = ngates)

      het[[i]] <- het.test(
        T   = Tcv,
        tau = fit_ml[[i]]$tau,
        Y   = Ycv,
        ngates = ngates)
    }


    # return a list of consist and het
    out <- list(consist = consist, het = het)

  }

  ## =================================
  ## cross validation
  ## =================================

  if(cv == TRUE){
    cat('Conduct hypothesis tests for GATEs unde cross-validation ...\n')

    # create empty lists to for consistcv and hetcv
    consistcv <- list()
    hetcv <- list()

    # run consistency and heterogeneity tests for each model
    for (i in algorithms) {

      consistcv[[i]] <- consistcv.test(
        T   = Tcv,
        tau = gettaucv(fit)[[i]],
        Y   = Ycv,
        ind = indcv,
        ngates = ngates)

      hetcv[[i]] <- hetcv.test(
        T   = Tcv,
        tau = gettaucv(fit)[[i]],
        Y   = Ycv,
        ind = indcv,
        ngates = ngates)

    }

  # return a list of consistcv and hetcv
  out <- list(consistcv = consistcv, hetcv = hetcv)
  }

  class(out) <- c("test_itr", class(out))

  return(out)

}


utils::globalVariables(c("T", "aupec", "sd", "pval", "Pval", "aupec.y", "fraction", "AUPECmin", "AUPECmax", ".", "fit", "out", "pape", "alg", "papep", "papd", "type", "gate", "group", "qnorm", "vec", "Y", "algorithm", "statistic", "p.value"))

