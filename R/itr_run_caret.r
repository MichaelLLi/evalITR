
## caret package

run_caret <- function(
  dat_train,
  dat_test,
  dat_total,
  params,
  indcv,
  iter,
  budget,
  train_method,
  c_threshold,
  meta_learner,
  ...
) {

  # split/cross-validation
  cv <- params$cv

  # run meta-learner
  meta_learner <- params$meta_learner

  # caret train parameters
  train_params <- params$train_params

  ## train
  fit_train <- train_caret(dat_train, train_params, train_method, meta_learner, ...)

  ## test
  fit_test <- test_caret(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv, c_threshold, meta_learner
  )

  return(list(test = fit_test, train = fit_train))
}

# train with caret
#' @importFrom stats as.formula
#' @importFrom dplyr select
train_caret <- function(dat_train, train_params, train_method, meta_learner, ...) {

  ## format training data
  training_data_elements_caret = create_ml_args_caret(dat_train)

  ## format total data
  total_data_elements_caret   = create_ml_args_caret(dat_total)

  ## train formula
  covariates = training_data_elements_caret[["data"]] %>% dplyr::select(-c(Y, T)) %>% colnames()

  formula_Y = as.formula(paste("Y ~ (", paste0(covariates, collapse = "+"), ")*T"))

  formula_ps = as.formula(paste("T ~ (", paste0(covariates, collapse = "+"), ")"))

  ## add additional parameters from ...
  train_params = c(train_params, list(...))
  
  ## train with specified meta-learner
  # slearner
  if (meta_learner == "slearner") {

    fit <- fit_slearner(
      data = training_data_elements_caret[["data"]],
      formula = formula_Y,
      train_method = train_method,
      train_params = train_params
    )
  }

  # tlearner
  if (meta_learner == "tlearner") {
    # treated group
    fit <- fit_tlearner(
      data = training_data_elements_caret[["data"]],
      formula = formula_Y,
      train_method = train_method,
      train_params = train_params
    )
  }

  # xlearner
  if (meta_learner == "xlearner") {

    fit <- fit_xlearner(
      data = training_data_elements_caret[["data"]],
      formula = formula_Y,
      train_method = train_method,
      train_params = train_params,
      covariates = covariates
    )
  }

  # rlearner
  if (meta_learner == "rlearner") {

    fit <- fit_rlearner(
      data = training_data_elements_caret[["data"]],
      formula_Y = formula,
      formula_ps = formula,
      train_method = train_method,
      train_params = train_params,
      covariates = covariates
    )
  }

  # drlearner
  if (meta_learner == "drlearner") {

    fit <- fit_drlearner(
      total_data = total_data_elements_caret[["data"]],
      train_data = training_data_elements_caret[["data"]],
      formula_Y = formula,
      formula_ps = formula,
      train_method = train_method,
      train_params = train_params,
      covariates = covariates
    )
  }

  return(fit)
}

#'@importFrom stats predict runif
test_caret <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv,
  iter, budget, cv, c_threshold, meta_learner
) {

  ## format data
  testing_data_elements_caret = create_ml_args_caret(dat_test)
  total_data_elements_caret   = create_ml_args_caret(dat_total)

  if(cv == TRUE){

    # test with specified meta-learner

    # slearner
    if (meta_learner == "slearner") {

    tau_total = predict_slearner(
      fit_train, 
      total_data_elements_caret[["data0t"]], total_data_elements_caret[["data1t"]], 
      n_df, cv)

    }

    # tlearner  
    if (meta_learner == "tlearner") {

    tau_total = predict_tlearner(
      fit_train, 
      total_data_elements_caret[["data"]], 
      n_df, cv)

    }

    # xlearner
    if (meta_learner == "xlearner") {

    tau_total = predict_xlearner(
      fit_train, 
      total_data_elements_caret[["data"]], 
      n_df, cv)

    }

    # rlearner
    if (meta_learner == "rlearner") {

    tau_total = predict_rlearner(
      fit_train, 
      total_data_elements_caret[["data"]], 
      n_df, cv)

    }

    # drlearner
    if (meta_learner == "drlearner") {

    tau_total = predict_drlearner(
      fit_Y_treated = fit_train$fit_Y_treated,
      fit_Y_control = fit_train$fit_Y_control,
      fit_ps = fit_train$fit_ps,
      total_data_elements_caret[["data"]], 
      n_df, cv)

    }

    ## compute quantities of interest
    tau_test <-  tau_total[indcv == iter]
    That     <-  as.numeric(tau_total > c_threshold)
    That_p   <- as.numeric(tau_total >= sort(tau_test, decreasing = TRUE)[floor(budget*length(tau_test))+1])

    ## output
    cf_output <- list(
      tau      = c(tau_test, rep(NA, length(tau_total) - length(tau_test))),
      tau_cv   = tau_total,
      That_cv  = That,
      That_pcv = That_p
      )
  }

  if(cv == FALSE){

    # test with specified meta-learner
    # slearner
    if (meta_learner == "slearner") {

      tau_test = predict_slearner(
        fit_train, 
        testing_data_elements_caret[["data0t"]], testing_data_elements_caret[["data1t"]], 
        n_df, cv)

    }

    # tlearner
    if (meta_learner == "tlearner") {

      tau_test = predict_tlearner(
        fit_train, 
        testing_data_elements_caret[["data"]], 
        n_df, cv)

    }

    # xlearner
    if (meta_learner == "xlearner") {

      tau_test = predict_xlearner(
        fit_train, 
        testing_data_elements_caret[["data"]], 
        n_df, cv)

    }

    # rlearner
    if (meta_learner == "rlearner") {

      tau_test = predict_rlearner(
        fit_train, 
        testing_data_elements_caret[["data"]], 
        n_df, cv)

    }

    # drlearner
    if (meta_learner == "drlearner") {

      tau_test = predict_drlearner(
        fit_train, 
        testing_data_elements_caret[["data"]], 
        n_df, cv)

    }
    
    ## compute quantities of interest
    That     =  as.numeric(tau_test > c_threshold)
    That_p   = numeric(length(That))
    That_p[sort(tau_test,decreasing =TRUE,index.return=TRUE)$ix[1:(floor(budget*length(tau_test))+1)]] = 1

    ## output
    cf_output <- list(
      tau      = tau_test,
      tau_cv   = tau_test,
      That_cv  = That,
      That_pcv = That_p
      )
  }

  return(cf_output)
}





