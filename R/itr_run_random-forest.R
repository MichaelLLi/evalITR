

## random forest

run_random_forest <- function(
  dat_train,
  dat_test,
  dat_total,
  params,
  indcv,
  iter,
  budget
) {

  # split/cross-validation
  cv <- params$cv

  ## train
  fit_train <- train_random_forest(dat_train)

  ## test
  fit_test <- test_random_forest(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv
  )

  return(list(test = fit_test, train = fit_train))
}



train_random_forest <- function(dat_train) {

  ## format training data
  training_data_elements_rf = create_ml_args_rf(dat_train)

  ## train formula
  formula_rf = training_data_elements_rf[["formula"]]

  ## fit
  fit <- randomForest::randomForest(formula_rf, data = training_data_elements_rf[["data"]], ntree = 500)

  return(fit)
}

#'@importFrom stats predict runif
test_random_forest <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, budget, cv
) {

  ## format data
  testing_data_elements_rf = create_ml_args_rf(dat_test)
  total_data_elements_rf   = create_ml_args_rf(dat_total)

  ## outcome
  outcome = testing_data_elements_rf[["data"]][["Y"]]

  if(cv == TRUE){

    if(length(unique(outcome)) > 2){

    ## predict
    Y0t_total = predict(
      fit_train,
      newdata = total_data_elements_rf[["data0t"]])
    Y1t_total = predict(
      fit_train,
      newdata = total_data_elements_rf[["data1t"]])

    }else {

    ## predict
    Y0t_total = predict(
      fit_train,
      newdata = total_data_elements_rf[["data0t"]],
      type = "prob")[, 2]
    Y1t_total = predict(
      fit_train,
      newdata = total_data_elements_rf[["data1t"]],
      type = "prob")[, 2]
    }

    tau_total=Y1t_total - Y0t_total + runif(n_df,-1e-6,1e-6)

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

    if(length(unique(outcome)) > 2){

    ## predict
    Y0t_test = predict(
      fit_train,
      newdata = testing_data_elements_rf[["data0t"]])
    Y1t_test = predict(
      fit_train,
      newdata = testing_data_elements_rf[["data1t"]])

    }else {

    ## predict
    Y0t_test = predict(
      fit_train,
      newdata = testing_data_elements_rf[["data0t"]],
      type = "prob")[, 2]
    Y1t_test = predict(
      fit_train,
      newdata = testing_data_elements_rf[["data1t"]],
      type = "prob")[, 2]
    }

    tau_test=Y1t_test - Y0t_test

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






