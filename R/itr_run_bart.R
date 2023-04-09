#'
run_bartc <- function(
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
  fit_train <- train_bartc(dat_train)

  ## test
  fit_test <- test_bartc(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv
  )

  return(list(test = fit_test, train = fit_train))
}



train_bartc <- function(dat_train) {

  ## format training data
  training_data_elements_bartc = create_ml_args_bartc(dat_train)

  ## fit
  fit <- bartCause::bartc(response = training_data_elements_bartc[["Y"]],
                      treatment = training_data_elements_bartc[["Treat"]],
                      confounders = training_data_elements_bartc[["X"]],
                      keepTrees = TRUE)

  return(fit)
}

#'@importFrom stats predict runif
test_bartc <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, budget, cv
) {

  ## format data
  testing_data_elements_bartc = create_ml_args_bartc(dat_test)
  total_data_elements_bartc = create_ml_args_bartc(dat_total)

  if(cv == TRUE){
    ## predict
    Y0t_total=predict(fit_train,total_data_elements_bartc[["X0t"]])
    Y1t_total=predict(fit_train,total_data_elements_bartc[["X1t"]])

    tau_total=colMeans(Y1t_total)-colMeans(Y0t_total) + runif(n_df,-1e-6,1e-6)


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
    Y0t_test=predict(fit_train,testing_data_elements_bartc[["X0t"]])
    Y1t_test=predict(fit_train,testing_data_elements_bartc[["X1t"]])

    tau_test=colMeans(Y1t_test)-colMeans(Y0t_test)

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

# Y0t<-predict(barc1,X0t)
# Y1t<-predict(barc1,X1t)
# tau_test2<-Y1t-Y0t
# That2=as.numeric(tau_test2>0)
