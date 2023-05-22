
## user-defined functions

run_user <- function(
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

  ## train
  fit_train <- train_user(dat_train, train_method)

  ## test
  fit_test <- test_user(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv
  )


  return(list(test = fit_test, train = fit_train))
}



train_user <- function(dat_train, train_method) {

  ## format training data
#   training_data_elements <- create_ml_args(dat_train)

  ## parameters
#   outcome = training_data_elements[["Y"]]
#   treatment = training_data_elements[["T"]]
#   covs  = training_data_elements[["X"]] %>% as.matrix()

  ## fit
  fit <- do.call(train_method, list(dat_train))

  return(fit)
}

#'@importFrom stats predict runif
test_user <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, budget, cv
) {

  ## format data
#   testing_data_elements <- create_ml_args(dat_test)
#   total_data_elements   <- create_ml_args(dat_total)

  if(cv == TRUE){
    # predict
    tau_total= do.call("predict", list(fit_train,dat_total))

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
    tau_test= do.call("predict", list(fit_train, dat_test))

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


