
## boosted tree

run_boost <- function(
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
  fit_train <- train_boost(dat_train)
  

  ## test 
  fit_test <- test_boost(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb, 
    indcv, iter, plim
  )
  
  return(fit_test)
}



train_boost <- function(dat_train) {
  
  ## format training data 
  training_data_elements_boosted = create_ml_args_boosted(dat_train)
  
  ## train formula
  formula_boosted = training_data_elements_boosted[["formula"]] 

  ## outcome
  outcome = training_data_elements_boosted[["data"]][["Y"]]

  if(length(unique(outcome)) > 2){
      ## fit
      fit <- gbm::gbm(formula_boosted, data = training_data_elements_boosted[["data"]],
                        distribution = "gaussian",
                        n.trees = 5000,
                        interaction.depth = 4)
  }else {
     ## fit
      fit <- gbm::gbm(formula_boosted, data = training_data_elements_boosted[["data"]],
                        distribution = "bernoulli",
                        n.trees = 5000,
                        interaction.depth = 4)
  }

  return(fit)

}

#'@importFrom stats predict runif
test_boost <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim
) {
  
  ## format data 
  testing_data_elements_boosted = create_ml_args_boosted(dat_test)
  total_data_elements_boosted   = create_ml_args_boosted(dat_total)
  
  ## predict 
  
  Y0t_total=predict(fit_train, as.data.frame(total_data_elements_boosted[["data0t"]]))
  Y1t_total=predict(fit_train, as.data.frame(total_data_elements_boosted[["data1t"]]))

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




  
