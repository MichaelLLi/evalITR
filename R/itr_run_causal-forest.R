

## causal-forest 

run_causal_forest <- function(
  dat_train, 
  dat_test, 
  dat_total,
  params, 
  indcv, 
  iter,
  plim
) {
  
  ## train 
  fit_train <- train_causal_forest(dat_train)

  ## test 
  fit_test <- test_causal_forest(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb, 
    indcv, iter, plim
  )

  return(fit_test)
}



train_causal_forest <- function(dat_train) {
  
  ## format training data 
  training_data_elements_cf <- create_ml_args_causalforest(dat_train)
  
  ## fit
  fit <- grf::causal_forest(
    training_data_elements_cf[["X_expand"]],
    training_data_elements_cf[["Y"]],
    training_data_elements_cf[["Treat"]],
    ci.group.size = 1,
    sample.fraction = 0.2,
    num.trees = 2000
  )
  return(fit)
}

#'@importFrom stats predict runif
test_causal_forest <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim
) {
  
  ## format data 
  testing_data_elements_cf <- create_ml_args_causalforest(dat_test)
  total_data_elements_cf   <- create_ml_args_causalforest(dat_total)
  
  ## predict 
  tau_total <- predict(
    fit_train,
    total_data_elements_cf[["X_expand"]]
  )$predictions + runif(n_df,-1e-6,1e-6)
  
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

