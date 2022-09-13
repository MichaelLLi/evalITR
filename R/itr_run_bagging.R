

## bagging

run_bagging <- function(
  dat_train, 
  dat_test, 
  dat_total,
  params, 
  indcv, 
  iter,
  plim,
  plot
) {
  
  ## train 
  fit_train <- train_bagging(dat_train)
  

  ## test 
  fit_test <- test_bagging(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb, 
    indcv, iter, plim
  )
  
  return(fit_test)
}


train_bagging <- function(dat_train) {
  
  ## format training data 
  training_data_elements_bagging = create_ml_args_bagging(dat_train)
  
  ## train formula
  formula_bagging = training_data_elements_bagging[["formula"]]
  
  ## tunning parameter
  tune_parameter = ncol(training_data_elements_bagging[["data"]]) -1

  ## fit
  fit <- randomForest::randomForest(formula_bagging, 
                      data = training_data_elements_bagging[["data"]],
                      mtry=tune_parameter, ntree = 500)
  
  return(fit)

}

#'@importFrom stats predict runif
test_bagging <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim
) {
  
  ## format data 
  testing_data_elements_bagging = create_ml_args_bagging(dat_test)
  total_data_elements_bagging   = create_ml_args_bagging(dat_total)
  
  ## predict 
  
  Y0t_total = predict(fit_train, newdata=total_data_elements_bagging[["data0t"]])
  Y1t_total = predict(fit_train, newdata=total_data_elements_bagging[["data1t"]])

  tau_total=Y1t_total - Y0t_total + runif(n_df,-1e-6,1e-6)


  ## compute quantities of interest 
  tau_test <-  tau_total[indcv == iter] 
  That     <-  as.numeric(tau_total > 0)
  That_p   <- as.numeric(tau_total >= sort(tau_test, decreasing = TRUE)[floor(plim*length(tau_test))+1])
  
  
  ## output 
  cf_output <- list(
    tau      = c(tau_test, rep(NA, length(tau_total) - length(tau_test))),
    tau_cv   = tau_total, 
    That_cv  = That, 
    That_pcv = That_p
  )
  
  return(cf_output)
}

  
