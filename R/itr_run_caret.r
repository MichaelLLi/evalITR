
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
  ...
) {

  # split/cross-validation
  cv <- params$cv

  # caret train parameters
  train_params <- params$train_params

  ## train
  fit_train <- train_caret(dat_train, train_params, train_method, ...)

  ## test
  fit_test <- test_caret(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv
  )

  return(list(test = fit_test, train = fit_train))
}



train_caret <- function(dat_train, train_params, train_method, ...) {

    ## format training data
    training_data_elements_caret = create_ml_args_caret(dat_train)

    ## train formula
    covariates = training_data_elements_caret[["data"]] %>% dplyr::select(-c(Y, T)) %>% colnames()

    formula = as.formula(paste("Y ~ (", paste0(covariates, collapse = "+"), ")*T"))

    ## add additional parameters from ...
    train_params = c(train_params, list(...))
    
    ## train
    fit <- do.call(caret::train, c(list(
            formula,
            data = training_data_elements_caret[["data"]],
            method = train_method), 
            train_params))

  return(fit)

}

#'@importFrom stats predict runif
test_caret <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv,
  iter, budget, cv
) {

  ## format data
  testing_data_elements_caret = create_ml_args_caret(dat_test)
  total_data_elements_caret   = create_ml_args_caret(dat_total)

  if(cv == TRUE){
    ## predict
    Y0t_total = predict(
      fit_train,
      as.data.frame(total_data_elements_caret[["data0t"]]),
      type = "raw")
    Y1t_total = predict(
      fit_train,
      as.data.frame(total_data_elements_caret[["data1t"]]),
      type = "raw")

    tau_total = Y1t_total - Y0t_total + runif(n_df,-1e-6,1e-6)

    ## compute quantities of interest
    tau_test <-  tau_total[indcv == iter]
    That     <-  as.numeric(tau_total > 0)
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
    ## predict
    Y0t_test = predict(
      fit_train,
      as.data.frame(testing_data_elements_caret[["data0t"]]),
      type = "raw")
    Y1t_test = predict(
      fit_train,
      as.data.frame(testing_data_elements_caret[["data1t"]]),
      type = "raw")

    tau_test = Y1t_test - Y0t_test

    ## compute quantities of interest
    That     =  as.numeric(tau_test > 0)
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





