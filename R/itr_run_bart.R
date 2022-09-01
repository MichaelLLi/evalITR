
## bartCause

run_bart <- function(
  dat_train, 
  dat_test, 
  dat_total,
  params, 
  indcv, 
  iter,
  plim,
  plot = FALSE
) {
  
  ## train 
  fit_train <- train_bart(dat_train)
  

  ## test 
  fit_test <- test_bart(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb, 
    indcv, iter, plim
  )
    
  return(fit_test)
}



train_bart <- function(dat_train) {
  
  ## format training data 
  training_data_elements_bartc = create_ml_args_bartc(dat_train)
  
  ## fit
  fit <- bartc(response = training_data_elements_bartc[["Y"]],
                      treatment = training_data_elements_bartc[["Treat"]],
                      confounders = training_data_elements_bartc[["X"]],
                      keepTrees = TRUE)

  return(fit)
}

test_bart <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim
) {
  
  ## format data 
  testing_data_elements_bartc = create_ml_args_bartc(dat_test)
  total_data_elements_bartc = create_ml_args_bartc(dat_total)
  
  ## predict 
  Y0t_total=predict(fit_train,total_data_elements_bartc[["X0t"]])
  Y1t_total=predict(fit_train,total_data_elements_bartc[["X1t"]])

  tau_total=colMeans(Y1t_total)-colMeans(Y0t_total) + runif(n_df,-1e-6,1e-6)


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

