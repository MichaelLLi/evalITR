# Split samples into training and testing data
split_samples = function(seed, data, train_prop, replace = FALSE){

  set.seed(seed)

  train_idx = sample(1:nrow(data), size = nrow(data)*train_prop, replace = replace)

  trainset = data[train_idx,]
  testset = data[-train_idx,]

  return(list(trainset = trainset, testset = testset))
}

#' Create arguments for ML algorithms
#' @importFrom stats as.formula
#' @param data A dataset
#' @param outcome Outcome of interests
#' @param treatment Treatment variable
create_ml_arguments = function(outcome, treatment, data){

  Y = data %>%
    dplyr::select(all_of(outcome)) %>% unlist()
  
  # Convert Y to factor if it's categorical (has 2 or fewer unique values)
  if(length(unique(Y)) <= 2) {
    Y = as.factor(Y)
  } else {
    Y = as.numeric(Y)
  }

  X = data %>%
    dplyr::select(-c(all_of(outcome), all_of(treatment))) %>%
    as.data.frame()

  T = data %>%
    dplyr::select(all_of(treatment)) %>% unlist() %>% as.numeric()

  formula = as.formula(paste(outcome, "~", paste(c(treatment, names(X)), collapse = "+")))

  return(list(Y = Y, X = X, T = T, formula = formula))
}

#' Create general arguments
#' @importFrom stats model.matrix
#' @param data A dataset
create_ml_args = function(data){

  Y = data[["Y"]]
  X = data[["X"]]
  T =data[["T"]]

  X_and_T = cbind(X, T)
  X_expand = model.matrix(~.*T, data = X_and_T)

  # also needed for testing:
  X0t = cbind(X, T = 0)
  X1t = cbind(X, T = 1)
  X0t_expand = model.matrix(~.*T, data = X0t)
  X1t_expand = model.matrix(~.*T, data = X1t)

  return(list(Y = Y, X = X, T = T, X_expand = X_expand, X0t_expand = X0t_expand, X1t_expand = X1t_expand))
}


#' Create arguments for causal forest
#' @importFrom stats model.matrix
#' @param data A dataset
create_ml_args_causalforest = function(data){

  Y = data[["Y"]]
  X = data[["X"]]
  T = data[["T"]]

  X_expand = model.matrix(~. -1, data = X)

  return(list(Y = Y, X = X, T = T, X_expand = X_expand))
}


#' Create arguments for bartMachine
#' @param data A dataset
create_ml_args_bart = function(data){

  Y = data[["Y"]]
  X = data[["X"]]
  T=data[["T"]]

  X_and_T = cbind(X, T)

  # also needed for testing:
  X0t = cbind(X, T = 0)
  X1t = cbind(X, T= 1)

  return(list(Y = Y, X = X, T = T, X_and_T = X_and_T, X0t = X0t, X1t = X1t))
}

#' Create arguments for bartCause
#' @param data A dataset
create_ml_args_bartc = function(data){

  Y = data[["Y"]]
  X = data[["X"]]
  T =data[["T"]]

  # also needed for testing:
  X0t = cbind(X, z = 0)
  X1t = cbind(X, z = 1)

  return(list(Y = Y, X = X, T = T, X0t = X0t, X1t = X1t))
}

#' Create arguments for LASSO
#' @importFrom stats model.matrix
#' @param data A dataset
create_ml_args_lasso = function(data){

  Y = data[["Y"]]
  X = data[["X"]]
  T =data[["T"]]

  X_and_T = cbind(X, T)
  X_expand = model.matrix(~.*T, data = X_and_T)

  # also needed for testing:
  X0t = cbind(X, T = 0)
  X1t = cbind(X, T = 1)
  X0t_expand = model.matrix(~.*T, data = X0t)
  X1t_expand = model.matrix(~.*T, data = X1t)

  return(list(Y = Y, X = X, T = T, X_expand = X_expand, X0t_expand = X0t_expand, X1t_expand = X1t_expand))
}


#' Create arguments for super learner
#' @importFrom stats model.matrix
#' @param data A dataset
create_ml_args_superLearner = function(data){

  Y = data[["Y"]]
  X = data[["X"]]
  T =data[["T"]]

  X_and_T = cbind(X, T)
  X_expand = model.matrix(~.*T, data = X_and_T) %>% as.data.frame()

  # also needed for testing:
  X0t = cbind(X, T = 0)
  X1t = cbind(X, T = 1)
  X0t_expand = model.matrix(~.*T, data = X0t) %>% as.data.frame()
  X1t_expand = model.matrix(~.*T, data = X1t) %>% as.data.frame()

  # remove intercept
  X_expand = X_expand[, -1]
  X0t_expand = X0t_expand[, -1]
  X1t_expand = X1t_expand[, -1]

  # change : in column names to avoid errors in super learner
  colnames(X_expand) = gsub(":", "_", colnames(X_expand))
  colnames(X0t_expand) = gsub(":", "_", colnames(X0t_expand))
  colnames(X1t_expand) = gsub(":", "_", colnames(X1t_expand))

  return(list(Y = Y, X = X, T = T, X_expand = X_expand, X0t_expand = X0t_expand, X1t_expand = X1t_expand))
}


#' Create arguments for SVM
#' @importFrom rlang .data
#' @param data A dataset
create_ml_args_svm = function(data){


  formula = data[["formula"]]
  Y = data[["Y"]] %>% scale()
  X = data[["X"]] %>% mutate_all(.data, scale)
  T = data[["T"]]  %>% scale()

  data = cbind(Y, X, T)

  # also needed for testing:
  data0t = cbind(Y, X, T = 0)
  data1t = cbind(Y, X, T = 1)

  return(list(formula = formula, data = data,
              data0t = data0t, data1t = data1t))
}

#' Create arguments for SVM classification
#' @importFrom stats as.formula
#' @param data A dataset
create_ml_args_svm_cls = function(data){


  Y = data[["Y"]]
  X = data[["X"]]
  T = data[["T"]]

  formula = as.formula(paste(as.factor("Y"), "~", paste(c("T", names(X)), collapse = "+")))

  data = cbind(Y, X, T)

  # also needed for testing:
  data0t = cbind(Y, X, T = 0)
  data1t = cbind(Y, X, T = 1)

  return(list(formula = formula, data = data,
              data0t = data0t, data1t = data1t))
}

# Create arguments for LDA
create_ml_args_lda = function(data){

  formula = data[["formula"]]
  Y = data[["Y"]]
  X = data[["X"]]
  T = data[["T"]]

  data = cbind(Y, X, T)

  # also needed for testing:
  X0t = cbind(X, T = 0)
  X1t = cbind(X, T = 1)
  data0t = cbind(Y, X0t)
  data1t = cbind(Y, X1t)

  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}

# Create arguments for boosted trees
create_ml_args_boosted = function(data){

  formula = data[["formula"]]
  Y = data[["Y"]]
  X = data[["X"]]
  T = data[["T"]]

  data = cbind(Y, X, T)

  # also needed for testing:
  data0t = cbind(Y, X, T = 0)
  data1t = cbind(Y, X, T = 1)

  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}


# Create arguments for random forest
create_ml_args_rf = function(data){

  Y = data[["Y"]]
  X = data[["X"]]
  T = data[["T"]]

  if(length(unique(Y)) > 2){
    formula = data[["formula"]]
  }else{
    formula = as.formula(paste("as.factor(Y) ~", paste(c("T", names(X)), collapse = "+")))
  }

  data = cbind(Y, X, T)

  # also needed for testing:
  data0t = cbind(Y, X, T = 0)
  data1t = cbind(Y, X, T = 1)

  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}


# Create arguments for bagging
create_ml_args_bagging = function(data){

  Y = data[["Y"]]
  X = data[["X"]]
  T = data[["T"]]

  if(length(unique(Y)) >2){
    formula = data[["formula"]]
  }else{
    formula = as.formula(paste("as.factor(Y) ~", paste(c("T", names(X)), collapse = "+")))
  }

  data = cbind(Y, X, T)

  # also needed for testing:
  data0t = cbind(Y, X, T = 0)
  data1t = cbind(Y, X, T = 1)

  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}


# Create arguments for CART
create_ml_args_cart = function(data){

  formula = data[["formula"]]
  Y = data[["Y"]]
  X = data[["X"]]
  T = data[["T"]]

  data = cbind(Y, X, T)

  # also needed for testing:
  data0t = cbind(Y, X, T = 0)
  data1t = cbind(Y, X, T = 1)

  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}



# Create arguments for caret
create_ml_args_caret = function(data){

  formula = data[["formula"]]
  Y = data[["Y"]]
  X = data[["X"]]
  T = data[["T"]]

  data = cbind(Y, X, T)

  # also needed for testing:
  data0t = cbind(Y, X, T = 0)
  data1t = cbind(Y, X, T = 1)

  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}


# function to fit slearner
fit_slearner_caret = function(data, formula, train_method, train_params){
  fit <- do.call(caret::train, c(list(
            formula,
            data = data,
            method = train_method), 
            train_params))

  return(fit)

}


# function to predict with slearner
predict_slearner = function(fit, data_0t, data_1t, n_df, cv){

  Y0t_total = predict(fit, as.data.frame(data_0t), type = "raw")
  Y1t_total = predict(fit, as.data.frame(data_1t), type = "raw")

  if(cv == TRUE){
    tau_total = Y1t_total - Y0t_total + runif(n_df,-1e-6,1e-6)
  }else{
    tau_total = Y1t_total - Y0t_total
  }

  return(tau_total)
}


# function to fit tlearner
fit_tlearner_caret = function(data, formula, train_method, train_params){

  treated_data = data %>% dplyr::filter(T == 1)
  control_data = data %>% dplyr::filter(T == 0)

  # for lasso, add a small amount of noise to the columns with zero variance
  if (train_method == "lasso") {
    # get the column names with zero variance
    zero_var_cols = caret::nearZeroVar(treated_data)
    # add a small amount of noise
    treated_data[, zero_var_cols] = treated_data[, zero_var_cols] + runif(nrow(treated_data), -1e-6, 1e-6)
    zero_var_cols = caret::nearZeroVar(control_data)
    # add a small amount of noise
    control_data[, zero_var_cols] = control_data[, zero_var_cols] + runif(nrow(control_data), -1e-6, 1e-6)
  }

  # treated group
  fit_treated <- do.call(caret::train, c(list(
          formula,
          data = treated_data,
          method = train_method), 
          train_params))

  # control group
  fit_control <- do.call(caret::train, c(list(
          formula,
          data = control_data,
          method = train_method), 
          train_params))

  return(list(fit_treated = fit_treated, fit_control = fit_control))

}


# function to predict with tlearner
predict_tlearner = function(fit_train, data , n_df, cv){

  Y0t_total = predict(
        fit_train$fit_control,
        as.data.frame(data),
        type = "raw")

  Y1t_total = predict(
    fit_train$fit_treated,
    as.data.frame(data),
    type = "raw")

  if(cv == TRUE){
    tau_total = Y1t_total - Y0t_total + runif(n_df,-1e-6,1e-6)
  }else{
    tau_total = Y1t_total - Y0t_total
  }

  return(tau_total)

}


# function to fit xlearner for caret
fit_xlearner_caret = function(data, formula, train_method, train_params, covariates, formula_ps){

  # treated group data
  data_treated <- data %>% dplyr::filter(T == 1)

  # control group data
  data_control <- data %>% dplyr::filter(T == 0)

  # for lasso, add a small amount of noise to the columns with zero variance
  if (train_method == "lasso") {
    # get the column names with zero variance
    zero_var_cols = caret::nearZeroVar(data_treated)
    # add a small amount of noise
    data_treated[, zero_var_cols] = data_treated[, zero_var_cols] + runif(nrow(data_treated), -1e-6, 1e-6)
    data_control[, zero_var_cols] = data_control[, zero_var_cols] + runif(nrow(data_control), -1e-6, 1e-6)
  }

  # treated group
  fit_treated_base <- do.call(caret::train, c(list(
          formula,
          data = data_treated,
          method = train_method), 
          train_params))

  # control group
  fit_control_base <- do.call(caret::train, c(list(
          formula,
          data = data_control,
          method = train_method), 
          train_params))

  # predict treated group with control model
  Y_hat_control_for_treated = predict(
    fit_control_base,
    as.data.frame(data_treated),
    type = "raw")

  # predict control group with treated model
  Y_hat_treated_for_control = predict(
    fit_treated_base,
    as.data.frame(data_control),
    type = "raw")

  # observed outcomes 
  Y_obs_treated = data_treated %>% dplyr::pull(Y)
  Y_obs_control = data_control %>% dplyr::pull(Y)

  # calculate residual
  D1 = Y_obs_treated - Y_hat_control_for_treated
  D0 = Y_hat_treated_for_control - Y_obs_control

  # formula
  formula_D1 = as.formula(paste("D1 ~ (", paste0(covariates, collapse = "+"), ")*T"))

  formula_D0 = as.formula(paste("D0 ~ (", paste0(covariates, collapse = "+"), ")*T"))

  # combine the data with the residuals
  new_data_treated = cbind(data_treated, D1)
  new_data_control = cbind(data_control, D0)

  # fit models on residuals
  fit_treated <- do.call(
    caret::train, c(list(
    formula_D1,
    data = as.data.frame(new_data_treated),
    method = train_method), 
    train_params))

  fit_control <- do.call(
    caret::train, c(list(
    formula_D0,
    data = as.data.frame(new_data_control),
    method = train_method), 
    train_params))

  # replace train_params$metric with "Accuracy"
  train_params$metric = "Accuracy"
  train_params$maximize = TRUE

  # get model info
  model_info <- caret::getModelInfo(model = train_method, regex = FALSE)[[1]]

  # check if the model allows for classification
  if (length(model_info$type) == 1) {
    
    # run propensity score model with lasso
    fit_ps <- cv.glmnet(
      data %>% dplyr::select(all_of(covariates)) %>% as.matrix(),
      data %>% dplyr::pull(T),
      family = "binomial",
      alpha = 1)  
        
  }

  # if the model allows for classification, run the propensity score model with the original method
  if (length(model_info$type) > 1) {

    # propensity score model
    fit_ps <- do.call(caret::train, c(list(
          formula_ps,
          data = data,
          method = train_method), 
          train_params))
  }

return(list(fit_treated = fit_treated, fit_control = fit_control, fit_ps = fit_ps, model_info = model_info))

}


# function to predict with xlearner
predict_xlearner = function(fit_train, data, n_df, cv, train_method){

  Y0t_total = predict(
    fit_train$fit_control,
    as.data.frame(data),
    type = "raw")

  Y1t_total = predict(
    fit_train$fit_treated,
    as.data.frame(data),
    type = "raw")

  # estimate propensity score following Künzel et al.(2019) SI. p.23
  model_info <- fit_train$model_info

  if (length(model_info$type) == 1) {
    p_score = predict(fit_train$fit_ps, data %>% dplyr::select(all_of(covariates)) %>% as.matrix(), type = "response")
  }

  if (length(model_info$type) > 1) {
    p_score_all = predict(fit_train$fit_ps, as.data.frame(data), type = "prob")
    
    # get the probabilities for treatment
    p_score = p_score_all[,"1"]
  }


  if(cv == TRUE){
    tau_total = p_score * (1 - Y1t_total) + p_score * Y0t_total + runif(n_df,-1e-6,1e-6)
  }else{
    tau_total = p_score * (1 - Y1t_total) + p_score * Y0t_total
  }

  return(tau_total)
}



# function to fit rlearner
fit_rlearner_caret = function(data, formula_Y, formula_ps, train_method, train_params, covariates){

  # outcome model
  fit_Y <- do.call(caret::train, c(list(
          formula_Y,
          data = data,
          method = train_method), 
          train_params))
  
  Y_hat = predict(fit_Y, as.data.frame(data), type = "raw")

  # replace train_params$metric with "Accuracy"
  train_params$metric = "Accuracy"
  train_params$maximize = TRUE

  # propensity score model
  fit_ps <- do.call(caret::train, c(list(
          formula_ps,
          data = data,
          method = train_method), 
          train_params))

  ps_hat = predict(fit_ps, as.data.frame(data), type = "prob")

  # get the probabilities for treatment
  ps_hat = ps_hat[,"1"]

  # calculate the weights
  y_tilde = Y_hat - Y_hat
  ps_tilde = ps_hat - ps_hat
  pseudo_outcome = y_tilde/ps_tilde
  weights = ps_tilde^2

  # combine the data with the pseudo-outcome
  new_data = cbind(data, pseudo_outcome)

  # formula
  formula_Y_pseudo = as.formula(paste("pseudo_outcome ~ (", paste0(covariates, collapse = "+"), ")*T"))

  # fit the outcome model on the weighted data
  fit_Y_weighted <- do.call(caret::train, c(list(
          formula_Y_pseudo,
          data = data,
          weights = weights,
          method = train_method), 
          train_params))

  return(fit_Y_weighted)

}

# function to predict with rlearner
predict_rlearner = function(fit_train, data, n_df, cv){

  Y_hat_weighted = predict(fit_train, as.data.frame(data), type = "raw")

  if(cv == TRUE){
    tau_total = Y_hat_weighted + runif(n_df,-1e-6,1e-6)
  }else{
    tau_total = Y_hat_weighted
  }

  return(tau_total)

}

# function to fit drlearner
fit_drlearner_caret = function(total_data, train_data, formula_Y, formula_ps, train_method, train_params, covariates){

  # replace train_params$metric with "Accuracy"
  train_params$metric = "Accuracy"
  train_params$maximize = TRUE

  # propensity score model
  fit_ps <- do.call(caret::train, c(list(
          formula_ps,
          data = train_data,
          method = train_method), 
          train_params))

  # outcome model for treated group
  fit_Y_treated <- do.call(caret::train, c(list(
          formula_Y,
          data = train_data %>% dplyr::filter(T == 1),
          method = train_method), 
          train_params))


  # outcome model for control group
  fit_Y_control <- do.call(caret::train, c(list(
          formula_Y,
          data = train_data %>% dplyr::filter(T == 0),
          method = train_method), 
          train_params))

  return(list(fit_Y_treated = fit_Y_treated, fit_Y_control = fit_Y_control, fit_ps = fit_ps))

}

# function to predict with drlearner
predict_drlearner_caret = function(fit_Y_treated, fit_Y_control, fit_ps, data, covariates, train_method, train_params, n_df, cv){

  # get predicted values
  ps_hat = predict(fit_ps, as.data.frame(data), type = "prob")

  mu1_hat = predict(fit_Y_treated, as.data.frame(data), type = "raw")
  mu0_hat = predict(fit_Y_control, as.data.frame(data), type = "raw")

  # pseudo-outcome regression
  psi_pseudo <- ((data$T - ps_hat) / (ps_hat * (1 - ps_hat))) * (data$Y - data$T * mu1_hat - (1 - data$T) * mu0_hat) + mu1_hat - mu0_hat

  # formula
  formula_psi_pseudo = as.formula(paste("psi_pseudo ~ (", paste0(covariates, collapse = "+"), ")*T"))

  # combine the data with the pseudo-outcome
  new_data = cbind(data, psi_pseudo)

  # fit the outcome model with test data
  fit_Y_pseudo <- do.call(caret::train, c(list(
          formula_psi_pseudo,
          data = new_data,
          method = train_method), 
          train_params))

  # get the predicted values
  tau_hat <- predict(fit_Y_pseudo, as.data.frame(new_data), type = "raw")

  if(cv == TRUE){
    tau_hat = tau_hat + runif(n_df,-1e-6,1e-6)
  }else{
    tau_hat = tau_hat
  }

  return(tau_hat)

}


# Re-organize cross-validation output to plot the AUPEC curve
getAupecOutput = function(
  tauML, taucvML, That_pcv_mat, MLname,
  NFOLDS, Ycv, Tcv, indcv
){
  aupec_grid = list()
  Ycv = as.numeric(Ycv)

  for (j in 1:NFOLDS){
    tau = tauML[,j][!is.na(tauML[,j])]
    aupec_grid[[j]] = AUPEC(Tcv[indcv==j], tau,Ycv[indcv==j])
  }

  ## use taucv
  aupec_cv = AUPECcv(T = Tcv, tau = taucvML, Y = Ycv, ind = indcv)

  aupec_vec = data.frame(matrix(NA, ncol = NFOLDS, nrow = max(table(indcv))))

  # forward fill the last aupec values
  for (j in 1:NFOLDS) {
    vec_length <- length(aupec_grid[[j]]$vec)
    fill_length <- nrow(aupec_vec) - vec_length

    if (fill_length > 0) {
      last_value <- aupec_grid[[j]]$vec[vec_length]
      extended_vec <- c(aupec_grid[[j]]$vec, rep(last_value, fill_length))
    } else {
      extended_vec <- aupec_grid[[j]]$vec
    }
    aupec_vec[,j] = extended_vec
  }

  # # fill the rest with NA
  # for (j in 1:NFOLDS){
  #   aupec_vec[,j] = c(aupec_grid[[j]]$vec, rep(NA, nrow(aupec_vec) - length(aupec_grid[[j]]$vec)))
  # }

  aupec_vec = rowMeans(aupec_vec, na.rm = T)
  outputdf = data.frame(
    type = rep(MLname,length(aupec_vec)),
    fraction = seq(1,length(aupec_vec))/length(aupec_vec),
    aupec = aupec_vec + mean(Ycv))

  return(list(aupec_cv = aupec_cv,
              aupec_vec = aupec_vec,
              outputdf = outputdf))
}

# Re-organize cross-validation output to plot the AUPEC curve -- standalone function
get_aupec_cv = function(
  tau, tau_cv, 
  Ycv, Tcv, indcv
){
  aupec_grid = list()
  Ycv = as.numeric(Ycv)
  NFOLDS = length(unique(indcv))

  for (j in 1:NFOLDS){
    aupec_grid[[j]] = AUPEC(Tcv[indcv==j],tau[indcv==j],Ycv[indcv==j])
  }

  ## use taucv
  aupec_cv = AUPECcv(T = Tcv, tau = tau_cv, Y = Ycv, ind = indcv)

  aupec_vec = data.frame(matrix(NA, ncol = NFOLDS, nrow = max(table(indcv))))

  # forward fill the last aupec values
  for (j in 1:NFOLDS) {
    vec_length <- length(aupec_grid[[j]]$vec)
    fill_length <- nrow(aupec_vec) - vec_length

    if (fill_length > 0) {
      last_value <- aupec_grid[[j]]$vec[vec_length]
      extended_vec <- c(aupec_grid[[j]]$vec, rep(last_value, fill_length))
    } else {
      extended_vec <- aupec_grid[[j]]$vec
    }
    aupec_vec[,j] = extended_vec
  }

  # format the output
  aupec_vec = rowMeans(aupec_vec, na.rm = T)
  outputdf = data.frame(
    fraction = seq(1,length(aupec_vec))/length(aupec_vec),
    aupec = aupec_vec + mean(Ycv))

  out <- list(aupec_cv = aupec_cv,
              aupec_vec = aupec_vec,
              outputdf = outputdf)
  return(out)
}

# transformation function for taucv matrix
gettaucv <- function(
    fit,
    ...
){
  estimates <- fit$estimates
  fit_ml <- estimates$fit_ml
  n_folds <- estimates$params$n_folds
  tau_cv <- list()

  # for one model
  for (k in seq(n_folds)) {
    tau_cv[[k]] <- fit_ml[["causal_forest"]][[k]][["tau_cv"]]
  }

  # convert to a single matrix
  tau_cv <- do.call(cbind, tau_cv)

  return(tau_cv)

}



# rename the columns of the data frame with the interaction terms
rename_interaction_terms <- function(interaction_df){
  colnames(interaction_df) <- gsub(":", "_", colnames(interaction_df))
  colnames(interaction_df) <- gsub("\\*", "_", colnames(interaction_df))
  colnames(interaction_df) <- gsub("\\(", "", colnames(interaction_df))
  colnames(interaction_df) <- gsub("\\)", "", colnames(interaction_df))
  colnames(interaction_df) <- gsub("\\+", "_", colnames(interaction_df))
  return(interaction_df)
}



# function to convert formula and create new variables
convert_formula <- function(user_formula, data, treatment){

  # get the outcome variable name from the formula
  outcome <- all.vars(user_formula)[1]

  # get the covariates from the formula
  interaction_df <- model.matrix(user_formula, data)
  interaction_df <- rename_interaction_terms(interaction_df)

  # remove variable Intercept from covariates list by name
  covariates_vec <- colnames(interaction_df)
  covariates_vec <- covariates_vec[!covariates_vec %in% c("Intercept", paste0(treatment))]
  # combine the interaction_df with the original data frame
  new_data = data %>% dplyr::select(all_of(outcome))
  combined_data <- cbind(new_data, interaction_df)

  return(list(data = combined_data, covariates = covariates_vec, outcome = outcome))
}

