
## svm

run_svm <- function(
  dat_train, 
  dat_test, 
  dat_total,
  params, 
  indcv, 
  iter,
  plim
) {

  # split/cross-validation
  cv <- params$cv

  ## train 
  fit_train <- train_svm(dat_train)
  
  ## test 
  fit_test <- test_svm(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb, 
    indcv, iter, plim, cv
  )
  

  return(fit_test)
}



train_svm <- function(dat_train) {
  
  ## format training data 
  training_data_elements_svm <- create_ml_args_svm(dat_train)
  formula_svm = training_data_elements_svm[["formula"]]
  
  ## fit
  fit <- e1071::svm(formula_svm,
             data = training_data_elements_svm[["data"]], 
             gamma = 1, 
             cost = 1,
             scale = TRUE,
             epsolon = 0.1,
             type = "eps-regression") 

  # fit <- fit(formula_svm, 
  #           data=training_data_elements_svm[["data"]], 
  #           model="svm", 
  #           gamma = 1, 
  #           C = 1,
  #           scaled = TRUE,
  #           epsilon = 0.1,
  #           kpar = list(sigma = 1),
  #           type = "eps-svr")

  # fit.pred =function(fit,data) {return (predict(fit,data)) }
  # svm.imp <- Importance(fit, 
  #                       data=training_data_elements_svm[["data"]],
  #                       PRED = fit.pred, 
  #                       outindex = 1, 
  #                       method = "svm")

  # fit.tune <- tune(svm, 
  #             formula_svm,
  #             data = training_data_elements_svm[["data"]],
  #             ranges = list(
  #               cost = c(0.1,1,10,100,1000),
  #               gamma = c(0.0001,0.001,0.01,0.1,1)
  #             ))
  # fit <- fit.tune$best.model

  return(fit)
}

#'@importFrom stats predict runif
test_svm <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim, cv
) {
  
  ## format data 
  testing_data_elements_svm = create_ml_args_svm(dat_test)
  total_data_elements_svm   = create_ml_args_svm(dat_total)
  
  if(cv == TRUE){
    ## predict 
    Y0t1_total=predict(fit_train,total_data_elements_svm[["data0t"]])
    Y1t1_total=predict(fit_train,total_data_elements_svm[["data1t"]])
    
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
  }

  if(cv == FALSE){
    ## predict 
    Y0t1_test=predict(fit_train,testing_data_elements_svm[["data0t"]])
    Y1t1_test=predict(fit_train,testing_data_elements_svm[["data1t"]])
    
    tau_test=Y1t1_test-Y0t1_test
    
    ## compute quantities of interest 
    That     =  as.numeric(tau_test > 0)
    That_p   = numeric(length(That))
    That_p[sort(tau_test,decreasing =TRUE,index.return=TRUE)$ix[1:(floor(plim*length(tau_test))+1)]] = 1

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
