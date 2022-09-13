
## lasso

run_lasso <- function(
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
  fit_train <- train_lasso(dat_train)
  
  ## test 
  fit_test <- test_lasso(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb, 
    indcv, iter, plim
  )

  
  return(fit_test)
}



train_lasso <- function(dat_train) {
  
  ## format training data 
  training_data_elements_lasso <- create_ml_args_lasso(dat_train)
  
  ## fit
  fit <- glmnet::glmnet(training_data_elements_lasso[["X_expand"]],
                training_data_elements_lasso[["Y"]],
                alpha = 1,
                lambda = 0.05)

  return(fit)
}

#'@importFrom stats predict runif
test_lasso <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim
) {
  
  ## format data 
  testing_data_elements_lasso <- create_ml_args_lasso(dat_test)
  total_data_elements_lasso   <- create_ml_args_lasso(dat_total)
  
  ## predict 
  
  Y0t1_total=predict(fit_train,total_data_elements_lasso[["X0t_expand"]])
  Y1t1_total=predict(fit_train,total_data_elements_lasso[["X1t_expand"]])

  tau_total=Y1t1_total-Y0t1_total + runif(n_df,-1e-6,1e-6)

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


