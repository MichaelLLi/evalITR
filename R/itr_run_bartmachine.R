#'
run_bartmachine <- function(
  dat_train, 
  dat_test, 
  dat_total,
  params, 
  indcv, 
  iter,
  plim
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


#' @import haven
train_bart <- function(dat_train) {
  
  ## format training data 
  training_data_elements_bart = create_ml_args_bart(dat_train)

  ## format binary outcome
  outcome_bart = training_data_elements_bart[["Y"]]

  if(length(unique(outcome_bart)) == 2){
    outcome_bart = factor(outcome_bart, levels = c(1,0))
  }

  ## fit
  fit <- bartMachine::bartMachine(
            X=training_data_elements_bart[["X_and_Treat"]],
            y=outcome_bart,
            num_trees = 30, 
            run_in_sample = TRUE,
            serialize = TRUE)

  return(fit)
}

#'@importFrom stats predict runif
test_bart <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim
) {
  
  ## format data 
  testing_data_elements_bart = create_ml_args_bart(dat_test)
  total_data_elements_bart   = create_ml_args_bart(dat_total)
  
  ## predict 
  Y0t_total=predict(fit_train,total_data_elements_bart[["X0t"]])
  Y1t_total=predict(fit_train,total_data_elements_bart[["X1t"]])

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
