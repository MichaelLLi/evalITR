#' Evaluate ITR
#'
#' @param outcome Outcome variable (or a list of outcome variables). Only takes in numeric values for both continous outcomes and binary outcomes (0 or 1).
#' @param treatment Treatment variable
#' @param data
#'   A data frame that contains \code{outcome} and \code{treatment}.
#' @param algorithms
#'   List of machine learning algorithms.
#' @param budget
#'   Proportion of treated units.
#' @param n_folds
#'   Number of cross-validation folds. Default is 5.
#' @param covariates
#'   Covariates included in the model.
#' @param ratio
#'   Split ratio between train and test set under sample splitting. Default is 0.
#' @param ngates
#'   The number of groups to separate the data into. The groups are determined by tau. Default is 5.
#' @param trainControl_method caret parameter
#' @param number caret parameter
#' @param repeats caret parameter
#' @param allowParallel caret parameter
#' @param train_method caret parameter
#' @param preProcess caret parameter
#' @param weights caret parameter
#' @param metric caret parameter
#' @param maximize caret parameter
#' @param tuneGrid caret parameter
#' @param tuneLength caret parameter
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
    budget,
    n_folds = 5,
    ratio = 0,
    ngates = 5,
    trainControl_method = "boot",
    number = ifelse(grepl("cv", trainControl_method), 10, 25),
    repeats = ifelse(grepl("[d_]cv$", trainControl_method), 1, NA),
    # p = 0.75,
    # search = "grid",
    # initialWindow = NULL,
    # horizon = 1,
    # fixedWindow = TRUE,
    # skip = 0,
    # verboseIter = FALSE,
    # returnData = TRUE,
    # returnResamp = "final",
    # savePredictions = FALSE,
    # classProbs = FALSE,
    # # summaryFunction = caret::defaultSummary(), #might need to change this
    # # selectionFunction = "best",
    # preProcOptions = list(
    #   thresh = 0.95, ICAcomp = 3, k = 5, freqCut = 95/5, uniqueCut = 10, cutoff = 0.9),
    # sampling = NULL,
    # index = NULL,
    # indexOut = NULL,
    # indexFinal = NULL,
    # timingSamps = 0,
    # # predictionBounds = rep(FALSE, 2),
    # seeds = NA,
    # adaptive = list(min = 5, alpha = 0.05, method = "gls", complete = TRUE),
    # trim = FALSE,
    allowParallel = TRUE,
    train_method = "rf",
    preProcess = NULL,
    weights = NULL,
    metric = ifelse(is.factor(data[outcome]), "Accuracy", "RMSE"),
    maximize = ifelse(metric %in% c("RMSE", "logLoss", "MAE", "logLoss"), FALSE, TRUE),
    trControl = trainControl(),
    tuneGrid = NULL,
    tuneLength = 1
) {

  ## caret parameters
  trainControl_params <- list(
    trainControl_method = trainControl_method,
    number = number,
    repeats = repeats,
    # p = p,
    # search = search,
    # initialWindow = initialWindow,
    # horizon = horizon,
    # fixedWindow = fixedWindow,
    # skip = skip,
    # verboseIter = verboseIter,
    # returnData = returnData,
    # returnResamp = returnResamp,
    # # savePredictions = savePredictions,
    # classProbs = classProbs,
    # # summaryFunction = summaryFunction,
    # # selectionFunction = selectionFunction,
    # preProcOptions = preProcOptions,
    # sampling = sampling,
    # index = index,
    # indexOut = indexOut,
    # indexFinal = indexFinal,
    # timingSamps = timingSamps,
    # # predictionBounds = predictionBounds,
    # seeds = seeds,
    # adaptive = adaptive,
    # trim = trim,
    allowParalle = allowParallel)

  # caret train parameters
  train_params <- list(
    train_method = train_method,
    preProcess = preProcess,
    weights = weights,
    metric = metric,
    maximize = maximize,
    # trControl = trainControl(),
    tuneGrid = tuneGrid,
    tuneLength = tuneLength
  )

  ## number of algorithms
  n_alg <- length(algorithms)

  ## some working variables
  n_df <- nrow(data)
  n_X  <- length(data) - 1
  n_folds <- n_folds
  cv <- ifelse(ratio > 0, FALSE, TRUE)

  params <- list(
    n_df = n_df, n_folds = n_folds, n_alg = n_alg, ratio = ratio, ngates = ngates, cv = cv, trainControl_params = trainControl_params, train_params = train_params)

  df <- list(algorithms = algorithms, outcome = outcome, data = data, treatment = treatment)

  ## loop over all outcomes
  estimates <- vector("list", length = length(outcome))
  for (m in 1:length(outcome)) {

    ## data to use
    ## rename outcome and treatment variable
    data_filtered <- data %>%
      select(Y = !!sym(outcome[m]), Treat = !!sym(treatment), all_of(covariates))

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
    estimates[[m]] <- itr_single_outcome(
      data       = data_filtered,
      algorithms = algorithms,
      params     = params,
      folds      = folds,
      budget     = budget
    )

  }

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

itr_single_outcome <- function(
    data,
    algorithms,
    params,
    folds,
    budget
) {

  ## obj to store outputs
  fit_ml <- lapply(1:params$n_alg, function(x) vector("list", length = params$n_folds))
  names(fit_ml) <- algorithms


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
                                        p = params$ratio,
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
    if ("causal_forest" %in% algorithms) {
      fit_ml[["causal_forest"]] <- run_causal_forest(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        budget    = budget,
        indcv     = 1, #indcv and iter set to 1 for sample splitting
        iter      = 1
      )
    }

    if("lasso" %in% algorithms){
      fit_ml[["lasso"]] <- run_lasso(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
    }

    if("svm" %in% algorithms){
      fit_ml[["svm"]] <- run_svm(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
    }


    if("bartc" %in% algorithms){
      fit_ml[["bartc"]] <- run_bartc(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
    }

    if("bart" %in% algorithms){
      fit_ml[["bart"]] <- run_bartmachine(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
    }

    if("boost" %in% algorithms){
      fit_ml[["boost"]] <- run_boost(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
    }

    if("random_forest" %in% algorithms){
      fit_ml[["random_forest"]] <- run_random_forest(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
    }

    if("bagging" %in% algorithms){
      fit_ml[["bagging"]] <- run_bagging(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
    }

    if("cart" %in% algorithms){
      fit_ml[["cart"]] <- run_cart(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
    }

    if("caret" %in% algorithms){
      fit_ml[["caret"]] <- run_caret(
        dat_train = training_data_elements,
        dat_test  = testing_data_elements,
        dat_total = total_data_elements,
        params    = params,
        indcv     = 1,
        iter      = 1,
        budget    = budget
      )
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
      if ("causal_forest" %in% algorithms) {
        fit_ml[["causal_forest"]][[j]] <- run_causal_forest(
          dat_train = training_data_elements,
          dat_test  = testing_data_elements,
          dat_total = total_data_elements,
          params    = params,
          indcv     = indcv,
          iter      = j,
          budget    = budget
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
          budget    = budget
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
          budget    = budget
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
          budget    = budget
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
          budget    = budget
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
          budget    = budget
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
          budget    = budget
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
          budget    = budget
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
          budget    = budget
        )
      }

      if("caret" %in% algorithms){
        fit_ml[["caret"]][[j]] <- run_caret(
          dat_train = training_data_elements,
          dat_test  = testing_data_elements,
          dat_total = total_data_elements,
          params    = params,
          indcv     = indcv,
          iter      = j,
          budget    = budget
        )
      }

    } ## end of fold

  }

  return(list(
    params = params, fit_ml = fit_ml,
    Ycv = Ycv, Tcv = Tcv, indcv = indcv, budget = budget
  ))
}
#' Estimate quantity of interests
#' @param fit Fitted model. Usually an output from \code{run_itr}
#' @param ... Further arguments passed to the function.
#' @return An object of \code{itr} class
#' @export
estimate_itr <- function(fit, ...){

  estimates  <- fit$estimates
  cv         <- estimates[[1]]$params$cv
  df         <- fit$df
  algorithms <- fit$df$algorithms
  outcome    <- fit$df$outcome

  qoi        <- vector("list", length = length(outcome))

  ## loop over all outcomes
  for (m in 1:length(outcome)) {

    ## compute qoi
    qoi[[m]] <- compute_qoi(estimates[[m]], algorithms)

  }

  out <- list(
    qoi = qoi, cv = cv, df = df, estimates = estimates)

  class(out) <- c("itr", class(out))

  return(out)

  }

utils::globalVariables(c("Treat", "aupec", "sd", "Pval", "aupec.y", "fraction", "AUPECmin", "AUPECmax", ".", "fit", "out", "pape", "alg", "papep", "papd", "type", "gate", "group", "qnorm", "vec"))

