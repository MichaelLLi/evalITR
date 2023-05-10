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
    dplyr::select(all_of(outcome)) %>% unlist() %>% as.numeric()

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




# # Create arguments for neural net
#
# create_ml_args_neuralnet = function(training_data, create_ml_arguments_outputs){
#
#
#   formula = create_ml_arguments_outputs[["formula"]]
#   Y = create_ml_arguments_outputs[["Y"]]
#   X = create_ml_arguments_outputs[["X"]]
#   T = create_ml_arguments_outputs[["T"]]
#
#   max = apply(training_data, 2 , max)
#   min = apply(training_data, 2 , min)
#   scaled_data = as.data.frame(scale(training_data, center = min, scale = max - min))
#
#   # also needed for testing:
#   X0t = cbind(X, T = 0)
#   X1t = cbind(X, T = 1)
#   X0t_expand = model.matrix(~. -1, data = X0t)
#   X1t_expand = model.matrix(~. -1, data = X1t)
#
#   return(list(formula = formula, scaled_data = scaled_data, X0t_expand = X0t_expand, X1t_expand = X1t_expand))
# }


# Create arguments for kNN
create_ml_args_knn = function(create_ml_arguments_outputs){

  formula = create_ml_arguments_outputs[["formula"]]
  Y = create_ml_arguments_outputs[["Y"]]
  X = create_ml_arguments_outputs[["X"]]
  T = create_ml_arguments_outputs[["T"]]

  data = cbind(Y, X, T)

  # also needed for testing:
  X0t = cbind(X, T = 0)
  X1t = cbind(X, T = 1)
  data0t = cbind(Y, X0t)
  data1t = cbind(Y, X1t)

  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
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
  for (j in 1:NFOLDS){
    aupec_vec[,j] = c(aupec_grid[[j]]$vec, rep(NA, nrow(aupec_vec) - length(aupec_grid[[j]]$vec)))
  }

  aupec_vec = rowMeans(aupec_vec, na.rm = T)
  outputdf = data.frame(type = rep(MLname,length(aupec_vec)),
    fraction = seq(1,length(aupec_vec))/length(aupec_vec),
    aupec = aupec_vec + mean(Ycv))

  return(list(aupec_cv = aupec_cv,
              aupec_vec = aupec_vec,
              outputdf = outputdf))
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
  new_data = data %>% dplyr::select(c(outcome)) 
  combined_data <- bind_cols(new_data, interaction_df)

  return(list(data = combined_data, covariates = covariates_vec, outcome = outcome))
}

