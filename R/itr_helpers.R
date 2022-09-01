# This file creates helper functions to run itr

options(java.parameters = "-Xmx5g")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, hash, haven, ggplot2, ggthemes, Cairo,
               evalITR, 
               grf,         
               glmnet,      
               bartMachine, 
               bartCause,
               e1071,
               #MASS, 
               gbm, 
               SuperLearner,
               neuralnet,
               caret, 
               rpart, 
               randomForest,
               class,
               parallel,
               furrr,
               future.apply,
               haven,
               labelled,
               here,
               rminer,
               forcats,
               Hmisc)



# Split samples into training and testing data --------------------------------

split_samples = function(seed, data, train_prop, replace = FALSE){
  
  set.seed(seed)
  
  train_idx = sample(1:nrow(data), size = nrow(data)*train_prop, replace = replace)
  
  trainset = data[train_idx,]
  testset = data[-train_idx,]
  
  return(list(trainset = trainset, testset = testset))
}


# Create arguments for ML algorithms ------------------------------------------

create_ml_arguments = function(outcome_var, treatment_var, data){
  
  Y = data %>% 
    dplyr::select(all_of(outcome_var)) %>% unlist() %>% as.numeric()
  
  X = data %>%
    dplyr::select(-c(all_of(outcome_var), all_of(treatment_var))) %>%
    as.data.frame()
  
  Treat = data %>%
    dplyr::select(all_of(treatment_var)) %>% unlist() %>% as.numeric()
  
  formula = as.formula(paste(outcome_var, "~", paste(c(treatment_var, names(X)), collapse = "+")))
  
  return(list(Y = Y, X = X, Treat = Treat, formula = formula))
}




# Create arguments for causal forest ------------------------------------------

create_ml_args_causalforest = function(data){
  
  Y = data[["Y"]]
  X = data[["X"]]
  Treat = data[["Treat"]]
  
  X_expand = model.matrix(~. -1, data = X)
  
  return(list(Y = Y, X = X, Treat = Treat, X_expand = X_expand))
}

# Create arguments for BART ---------------------------------------------------

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

# Create arguments for bartCause ----------------------------------------------

create_ml_args_bartc = function(data){
  
  Y = data[["Y"]]
  X = data[["X"]]
  Treat =data[["Treat"]]
  
  # also needed for testing:
  X0t = cbind(X, z = 0)
  X1t = cbind(X, z = 1)
  
  return(list(Y = Y, X = X, Treat = Treat, X0t = X0t, X1t = X1t))
}

# Create arguments for LASSO --------------------------------------------------

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

# Create arguments for SVM ----------------------------------------------------

create_ml_args_svm = function(data){
  
  
  formula = data[["formula"]]
  Y = data[["Y"]] %>% scale()
  X = data[["X"]] %>% mutate_all(., scale)
  Treat = data[["Treat"]]  %>% scale()
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  data0t = cbind(Y, X, Treat = 0)
  data1t = cbind(Y, X, Treat = 1)

  return(list(formula = formula, data = data, 
              data0t = data0t, data1t = data1t))
}

# Create arguments for SVM classification -------------------------------------

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

# Create arguments for LDA ----------------------------------------------------

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

# Create arguments for boosted trees ------------------------------------------

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


# Create arguments for random forest ------------------------------------------

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

# Create argments for random forest for classification ------------------------

create_ml_args_rf_cls = function(data){
  
  Y = data[["Y"]]
  X = data[["X"]]
  Treat = data[["Treat"]]
  
  formula = as.formula(paste(as.factor("Y"), "~", paste(c("Treat", names(X)), collapse = "+")))
  
  data = cbind(Y, X, Treat)
  
  # also needed for testing:
  data0t = cbind(Y, X, Treat = 0)
  data1t = cbind(Y, X, Treat = 1)
  
  return(list(formula = formula, data = data, data0t = data0t, data1t = data1t))
}


# Create arguments for bagging ------------------------------------------

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


# Create arguments for CART ------------------------------------------

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


# # Create arguments for neural net ---------------------------------------------
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


# Create arguments for kNN ----------------------------------------------------

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


# Re-organize cross-validation output to plot the AUPEC curve ---------------------------------------------------- 

# getAupecOutput = function(tauML, taucvML, ThatpcvML, MLname, Ycv = Ycv, Tcv = Tcv, indcv = indcv){
#   aupec_grid = list()
#   for (j in 1:NFOLDS){
#     tau = tauML[,j][!is.na(tauML[,j])]
#     aupec_grid[[j]] = AUPEC(Tcv[indcv==j], tau,Ycv[indcv==j])
#   }
    
#   aupec_cv = AUPECcv(Tcv, ThatpcvML, Ycv, indcv)
  
#   aupec_vec = data.frame(matrix(NA, ncol = NFOLDS, nrow = max(table(indcv))))
#   for (j in 1:NFOLDS){
#     aupec_vec[,j] = c(aupec_grid[[j]]$vec, rep(NA, nrow(aupec_vec) - length(aupec_grid[[j]]$vec)))
#   }
  
#   aupec_vec = rowMeans(aupec_vec, na.rm = T)
#   outputdf = data.frame(type = rep(MLname,length(aupec_vec)), fraction = seq(1,length(aupec_vec))/length(aupec_vec), aupec = aupec_vec + mean(Ycv))
  
#   return(list(aupec_cv = aupec_cv,
#               aupec_vec = aupec_vec,
#               outputdf = outputdf))
# }



getAupecOutput = function(
  tauML, taucvML, That_pcv_mat, MLname,
  NFOLDS, Ycv, Tcv, indcv
){
  aupec_grid = list()
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

# Plot the AUPEC curve ---------------------------------------------------- 
plot_aupec <- function(fit, data, plot_name, algorithms){

graphLabels <- data.frame(type = algorithms,
                          Pval = bind_rows(map(fit[[1]]$AUPEC, ~.x$aupec_cv)) %>% 
                            mutate(Pval = paste0("AUPEC = ", round(aupec, 2), 
                                                 " (s.e. = ", round(sd, 2), ")")) %>% 
                            pull(Pval))

Tcv = data$Treat
Ycv = data$Y

bind_rows(map(fit[[1]]$AUPEC, ~.x$aupec_cv)) %>% 
  mutate(type = algorithms) %>%
  inner_join(bind_rows(
    map(fit[[1]]$AUPEC, ~.x$outputdf)),
    by = "type"
  ) %>%
  mutate(AUPECmin = aupec.y - 1.96*sd,
         AUPECmax = aupec.y + 1.96*sd) %>% {{temp <<-.}} %>% 
  ggplot(aes(x=fraction,y=aupec.y,group=type)) + geom_line(alpha=0.5,colour="red") + scale_colour_few("Dark")+
  xlab("Maximum Proportion Treated")+
  ylab("AUPEC")+
  facet_wrap(~type)+scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(limits = c(min(temp$AUPECmin, na.rm = TRUE)-0.5, max(temp$AUPECmax, na.rm = TRUE)+ 0.5))+ 
  theme_few()+ 
  geom_ribbon(aes(ymin=AUPECmin, ymax=AUPECmax),fill="tomato1",alpha=0.2) +
  geom_abline(intercept = sum(Ycv*(1-Tcv))/sum(1-Tcv), slope = sum(Ycv*Tcv)/sum(Tcv)-sum(Ycv*(1-Tcv))/sum(1-Tcv),size=0.5) +
  geom_text(data = graphLabels, aes(x = 0.57, y = max(temp$AUPECmax, na.rm = TRUE)+0.35, label = Pval),size=3) +
  theme(text = element_text(size=13.5),
        axis.text = element_text(size=10),
        strip.text = element_text(size = 13.5)) -> out

return(out)
}

