## superLearner

run_superLearner <- function(
  dat_train,
  dat_test,
  dat_total,
  params,
  indcv,
  iter,
  budget,
  train_method,
  SL_library,
  ...
) {

  # split/cross-validation
  cv <- params$cv

  ## train
  fit_train <- train_superLearner(
    dat_train, 
    train_method, 
    SL_library)

  ## test
  fit_test <- test_superLearner(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv
  )

  return(list(test = fit_test, train = fit_train))
}



train_superLearner <- function(dat_train, train_method, SL_library) {

  ## format training data
  training_data_elements <- create_ml_args_superLearner(dat_train)

  ## parameters
  Y = training_data_elements[["Y"]]
  X = training_data_elements[["X_expand"]]
  SL_library = SL_library

   if(length(unique(Y)) > 2){

      fit <- SuperLearner(
        Y = Y, 
        X = X, 
        family = gaussian(),
        SL.library = SL_library)

    }else {
      
      fit <- SuperLearner(
        Y = Y, 
        X = X, 
        family = binomial(),
        SL.library = SL_library)
  }

  return(fit)
}

#'@importFrom stats predict runif
test_superLearner <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, budget, cv
) {

  ## format data
  testing_data_elements <- create_ml_args_superLearner(dat_test)
  total_data_elements   <- create_ml_args_superLearner(dat_total)

  if(cv == TRUE){
    ## predict
    Y0t1_total = predict(
      fit_train,
      testing_data_elements[["X0t_expand"]],
      onlySL = TRUE)
    Y1t1_total = predict(
      fit_train,
      total_data_elements[["X1t_expand"]],
      onlySL = TRUE)

    tau_total=Y1t1_total$pred-Y0t1_total$pred + runif(n_df,-1e-6,1e-6)

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
    Y0t1_test = predict(
      fit_train,
      testing_data_elements[["X0t_expand"]],
      onlySL = TRUE)
    Y1t1_test = predict(
      fit_train,
      testing_data_elements[["X1t_expand"]],
      onlySL = TRUE)

    tau_test = Y1t1_test$pred - Y0t1_test$pred

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


