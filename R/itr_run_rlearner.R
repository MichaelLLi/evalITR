
## rlearner

run_rlearner <- function(
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
  fit_train <- train_rlearner(dat_train, train_method)

  ## test
  fit_test <- test_rlearner(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv
  )


  return(list(test = fit_test, train = fit_train))
}



train_rlearner <- function(dat_train, train_method) {

  ## format training data
  training_data_elements <- create_ml_args(dat_train)

  ## parameters
  outcome = training_data_elements[["Y"]]
  treat = training_data_elements[["Treat"]]
  covs  = training_data_elements[["X"]] %>% as.matrix()

  ## model
  rlearner_fun <- get(train_method, envir = as.environment("package:rlearner"))

  ## fit
  fit <- rlearner_fun(covs, treat, outcome)

  return(fit)
}

#'@importFrom stats predict runif
test_rlearner <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, budget, cv
) {

  ## format data
  testing_data_elements <- create_ml_args(dat_test)
  total_data_elements   <- create_ml_args(dat_total)

  if(cv == TRUE){
    ## predict
    new_data = total_data_elements[["X"]] %>% as.matrix()
    tau_total=predict(fit_train,new_data)

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
    new_data = testing_data_elements[["X"]] %>% as.matrix()
    tau_test=predict(fit_train, new_data)

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


