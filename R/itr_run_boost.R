
## boosted tree

run_boost <- function(
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
  fit_train <- train_boost(dat_train)

  ## test
  fit_test <- test_boost(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv
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
  fit_train, dat_test, dat_total, n_df, n_tb, indcv,
  iter, budget, cv
) {

  ## format data
  testing_data_elements_boosted = create_ml_args_boosted(dat_test)

  total_data_elements_boosted   = create_ml_args_boosted(dat_total)

  if(cv == TRUE){
    ## predict
    Y0t_total = predict(
      fit_train,
      as.data.frame(total_data_elements_boosted[["data0t"]]),
      type = "response")
    Y1t_total = predict(
      fit_train,
      as.data.frame(total_data_elements_boosted[["data1t"]]),
      type = "response")

    tau_total = Y1t_total - Y0t_total + runif(n_df,-1e-6,1e-6)

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
    Y0t_test = predict(
      fit_train,
      as.data.frame(testing_data_elements_boosted[["data0t"]]),
      type = "response")
    Y1t_test = predict(
      fit_train,
      as.data.frame(testing_data_elements_boosted[["data1t"]]),
      type = "response")

    tau_test = Y1t_test - Y0t_test

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





