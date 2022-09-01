#' @export
run_bartmachine <- function(
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
  fit_train <- train_bart(dat_train)
  

  ## test 
  fit_test <- test_bart(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb, 
    indcv, iter, plim
  )
    
  # plot
  if(plot == TRUE){
  plot <- plot_var_importance_bart(fit_train, iter)
  }else {
     plot <- NULL
  }  
  
  return(fit_test)
}



train_bart <- function(dat_train) {
  
  ## format training data 
  # training_data_elements_bartc = create_ml_args_bartc(dat_train)
  training_data_elements_bart = create_ml_args_bart(dat_train)

  ## fit
  fit <- bartMachine::bartMachine(
            X=training_data_elements_bart[["X_and_Treat"]],
            y=training_data_elements_bart[["Y"]],
            num_trees = 30, 
            run_in_sample = TRUE,
            serialize = TRUE)



  # fit <- bartc(response = training_data_elements_bartc[["Y"]],
  #                     treatment = training_data_elements_bartc[["Treat"]],
  #                     confounders = training_data_elements_bartc[["X"]],
  #                     keepTrees = TRUE)

  return(fit)
}

test_bart <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim
) {
  
  ## format data 
  # testing_data_elements_bartc = create_ml_args_bartc(dat_test)
  # total_data_elements_bartc = create_ml_args_bartc(dat_total)
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


## plot varaible importance
plot_var_importance_bart <- function(fit_train, fold){

  png(paste0("plot/bart_var_importance", fold, ".png"))
  investigate_var_importance(fit_train, plot = TRUE) 
  dev.off()

}
