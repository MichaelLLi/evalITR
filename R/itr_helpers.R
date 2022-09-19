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
  
  Treat = data %>%
    dplyr::select(all_of(treatment)) %>% unlist() %>% as.numeric()
  
  formula = as.formula(paste(outcome, "~", paste(c(treatment, names(X)), collapse = "+")))
  
  return(list(Y = Y, X = X, Treat = Treat, formula = formula))
}


#' Create arguments for causal forest 
#' @importFrom stats model.matrix
#' @param data A dataset
create_ml_args_causalforest = function(data){
  
  Y = data[["Y"]]
  X = data[["X"]]
  Treat = data[["Treat"]]
  
  X_expand = model.matrix(~. -1, data = X)
  
  return(list(Y = Y, X = X, Treat = Treat, X_expand = X_expand))
}


#' Create arguments for BART 
#' @param data A dataset
create_ml_args_bart = function(data){
  
  Y = data[["Y"]]
  X = data[["X"]]
  Treat =data[["Treat"]]
  
  X_and_Treat = cbind(X, Treat)
  
  # also needed for testing:
  X0t = cbind(X, Treat = 0)
  X1t = cbind(X, Treat = 1)
  
  return(list(Y = Y, X = X, Treat = Treat, X_and_Treat = X_and_Treat, X0t = X0t, X1t = X1t))
}

#' Create arguments for bartCause 
#' @param data A dataset
create_ml_args_bartc = function(data){
  
  Y = data[["Y"]]
  X = data[["X"]]
  Treat =data[["Treat"]]
  
  # also needed for testing:
  X0t = cbind(X, z = 0)
  X1t = cbind(X, z = 1)
  
  return(list(Y = Y, X = X, Treat = Treat, X0t = X0t, X1t = X1t))
}

#' Create arguments for LASSO 
#' @importFrom stats model.matrix
#' @param data A dataset
create_ml_args_lasso = function(data){
  
  Y = data[["Y"]]
  X = data[["X"]]
  Treat =data[["Treat"]]
  
  X_and_Treat = cbind(X, Treat)
  X_expand = model.matrix(~.*Treat, data = X_and_Treat)
  
  # also needed for testing:
  X0t = cbind(X, Treat = 0)
  X1t = cbind(X, Treat = 1)
  X0t_expand = model.matrix(~.*Treat, data = X0t)
  X1t_expand = model.matrix(~.*Treat, data = X1t)
  
  return(list(Y = Y, X = X, Treat = Treat, X_expand = X_expand, X0t_expand = X0t_expand, X1t_expand = X1t_expand))
}

#' Create arguments for SVM 
#' @importFrom rlang .data
#' @param data A dataset
create_ml_args_svm = function(data){
  
  
  formula = data[["formula"]]
  Y = data[["Y"]] %>% scale()
  X = data[["X"]] %>% mutate_all(.data, scale)
  Treat = data[["Treat"]]  %>% scale()
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  data0t = cbind(Y, X, Treat = 0)
  data1t = cbind(Y, X, Treat = 1)

  return(list(formula = formula, data = data, 
              data0t = data0t, data1t = data1t))
}

#' Create arguments for SVM classification 
#' @importFrom stats as.formula
#' @param data A dataset
create_ml_args_svm_cls = function(data){
  
  
  Y = data[["Y"]] 
  X = data[["X"]] 
  Treat = data[["Treat"]]  
  
  formula = as.formula(paste(as.factor("Y"), "~", paste(c("Treat", names(X)), collapse = "+")))
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  data0t = cbind(Y, X, Treat = 0)
  data1t = cbind(Y, X, Treat = 1)
  
  return(list(formula = formula, data = data, 
              data0t = data0t, data1t = data1t))
}

# Create arguments for LDA 
create_ml_args_lda = function(data){
  
  formula = data[["formula"]]
  Y = data[["Y"]]
  X = data[["X"]]
  Treat = data[["Treat"]]
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  X0t = cbind(X, Treat = 0)
  X1t = cbind(X, Treat = 1)
  data0t = cbind(Y, X0t)
  data1t = cbind(Y, X1t)
  
  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}

# Create arguments for boosted trees 
create_ml_args_boosted = function(data){
  
  formula = data[["formula"]]
  Y = data[["Y"]]
  X = data[["X"]]
  Treat = data[["Treat"]]
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  data0t = cbind(Y, X, Treat = 0)
  data1t = cbind(Y, X, Treat = 1)
  
  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}


# Create arguments for random forest 
create_ml_args_rf = function(data){
  
  formula = data[["formula"]]
  Y = data[["Y"]]
  X = data[["X"]]
  Treat = data[["Treat"]]
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  data0t = cbind(Y, X, Treat = 0)
  data1t = cbind(Y, X, Treat = 1)
  
  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}

#' Create argments for random forest (classification)
create_ml_args_rf_cls = function(data){
  
  Y = data[["Y"]]
  X = data[["X"]]
  Treat = data[["Treat"]]
  
  formula = as.formula(paste("as.factor(Y) ~", paste(c("Treat", names(X)), collapse = "+")))
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  data0t = cbind(Y, X, Treat = 0)
  data1t = cbind(Y, X, Treat = 1)
  
  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}

# Create arguments for bagging 
create_ml_args_bagging = function(data){
  
  formula = data[["formula"]]
  Y = data[["Y"]]
  X = data[["X"]]
  Treat = data[["Treat"]]
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  data0t = cbind(Y, X, Treat = 0)
  data1t = cbind(Y, X, Treat = 1)
  
  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}

# Create arguments for bagging (classification)
create_ml_args_bagging_cls = function(data){
  
  Y = data[["Y"]]
  X = data[["X"]]
  Treat = data[["Treat"]]

  formula = as.formula(paste("as.factor(Y) ~", paste(c("Treat", names(X)), collapse = "+")))  
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  data0t = cbind(Y, X, Treat = 0)
  data1t = cbind(Y, X, Treat = 1)
  
  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}


# Create arguments for CART 
create_ml_args_cart = function(data){
  
  formula = data[["formula"]]
  Y = data[["Y"]]
  X = data[["X"]]
  Treat = data[["Treat"]]
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  data0t = cbind(Y, X, Treat = 0)
  data1t = cbind(Y, X, Treat = 1)
  
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
#   Treat = create_ml_arguments_outputs[["Treat"]]
#   
#   max = apply(training_data, 2 , max)
#   min = apply(training_data, 2 , min)
#   scaled_data = as.data.frame(scale(training_data, center = min, scale = max - min))
#  
#   # also needed for testing:
#   X0t = cbind(X, Treat = 0)
#   X1t = cbind(X, Treat = 1)
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
  Treat = create_ml_arguments_outputs[["Treat"]]
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  X0t = cbind(X, Treat = 0)
  X1t = cbind(X, Treat = 1)
  data0t = cbind(Y, X0t)
  data1t = cbind(Y, X1t)
  
  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}

# convert predicted outcomes for CART
convert_outcome <- function(x, predict_outcome){
  if(predict_outcome[x,1] >= 0.5){
  outcome[x] = colnames(predict_outcome)[1] %>% as.numeric()
}else {
   outcome[x] = colnames(predict_outcome)[2] %>% as.numeric()
}
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

  ## use That_pcv_mat
  # aupec_cv = AUPECcv(T = Tcv, tau = That_pcv_mat, Y = Ycv, ind = indcv)

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

