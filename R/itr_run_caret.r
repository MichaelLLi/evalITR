
## caret package

run_caret <- function(
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

  # caret trainControl parameters
  trainControl_params <- params$trainControl_params

  # caret train parameters
  train_params <- params$train_params

  ## train
  fit_train <- train_caret(dat_train, trainControl_params, train_params)

  ## test
  fit_test <- test_caret(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv
  )

  return(fit_test)
}



train_caret <- function(dat_train, trainControl_params, train_params) {

    # caret trainControl parameters
    trainControl_method = trainControl_params$trainControl_method
    number = trainControl_params$number
    repeats = trainControl_params$repeats
    # p = trainControl_params$p
    # search = trainControl_params$search
    # initialWindow = trainControl_params$initialWindow
    # horizon = trainControl_params$horizon
    # fixedWindow = trainControl_params$fixedWindow
    # skip = trainControl_params$skip
    # verboseIter = trainControl_params$verboseIter
    # returnData = trainControl_params$returnData
    # returnResamp = trainControl_params$returnResamp
    # # savePredictions = trainControl_params$savePredictions
    # classProbs = trainControl_params$classProbs
    # # summaryFunction = trainControl_params$summaryFunction
    # # selectionFunction = trainControl_params$selectionFunction
    # preProcOptions = trainControl_params$preProcOptions
    # sampling = trainControl_params$sampling
    # index = trainControl_params$index
    # indexOut = trainControl_params$indexOut
    # indexFinal = trainControl_params$indexFinal
    # timingSamps = trainControl_params$timingSamps
    # # predictionBounds = trainControl_params$predictionBounds
    # seeds = trainControl_params$seeds
    # adaptive = trainControl_params$adaptive
    # trim = trainControl_params$trim
    allowParallel = trainControl_params$allowParallel

    # caret train paramters
    train_method = train_params$train_method
    preProcess = train_params$preProcess
    weights = train_params$weights
    metric = train_params$metric
    maximize = train_params$maximize
    # trControl = train_params$trainControl()
    tuneGrid = train_params$tuneGrid
    tuneLength = train_params$tuneLength

    ## format training data
    training_data_elements_caret = create_ml_args_caret(dat_train)

    ## train formula
    covariates = training_data_elements_caret[["data"]] %>% dplyr::select(-c(Y, Treat)) %>% colnames()

    formula = as.formula(paste("Y ~ (", paste0(covariates, collapse = "+"), ")*Treat"))

    # fitControl <- caret::trainControl(
    #     # 10-fold CV
    #     method = "repeatedcv",
    #     # method = "none")
    #     number = 5,
    #     ## repeated ten times
    #     repeats = 5)

    fitControl <- caret::trainControl(
        trainControl_method, number, repeats,
        # p, search, initialWindow, horizon, fixedWindow, skip, verboseIter, returnData, returnResamp,
        # # savePredictions,
        # classProbs,
        # # summaryFunction,
        # # selectionFunction,
        # preProcOptions, sampling, index, indexOut, indexFinal, timingSamps,
        # # predictionBounds,
        # seeds, adaptive, trim,
        allowParallel)


    ## fit
    # fit <- caret::train(formula,
    #                 data = training_data_elements_caret[["data"]],
    #                 method = "gbm",
    #                 trControl = fitControl,
    #                 ## This last option is actually one
    #                 ## for gbm() that passes through
    #                 verbose = FALSE)

    fit <- caret::train(formula,
                    data = training_data_elements_caret[["data"]],
                    method = train_method,
                    trControl = fitControl,
                    preProcess = preProcess,
                    weights = weights,
                    metric = metric,
                    maximize = maximize,
                    # trControl = train_params$trainControl()
                    tuneGrid = tuneGrid,
                    tuneLength = tuneLength,
                    ## This last option is actually one
                    ## for gbm() that passes through
                    verbose = FALSE)

  return(fit)

}

#'@importFrom stats predict runif
test_caret <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv,
  iter, budget, cv
) {

  ## format data
  testing_data_elements_caret = create_ml_args_caret(dat_test)
  total_data_elements_caret   = create_ml_args_caret(dat_total)

  if(cv == TRUE){
    ## predict
    Y0t_total = predict(
      fit_train,
      as.data.frame(total_data_elements_caret[["data0t"]]),
      type = "raw")
    Y1t_total = predict(
      fit_train,
      as.data.frame(total_data_elements_caret[["data1t"]]),
      type = "raw")

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
      as.data.frame(testing_data_elements_caret[["data0t"]]),
      type = "raw")
    Y1t_test = predict(
      fit_train,
      as.data.frame(testing_data_elements_caret[["data1t"]]),
      type = "raw")

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





