
## svm

run_svm <- function(
  dat_train, 
  dat_test, 
  dat_total,
  params, 
  indcv, 
  iter,
  plim,
  plot
) {
  
  ## train 
  fit_train <- train_svm(dat_train)
  
  ## test 
  fit_test <- test_svm(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb, 
    indcv, iter, plim
  )
  
  # plot
  # if(plot == TRUE){
  #   plot <- plot_var_importance_svm(dat_train, fit_train, "SVM", iter)
  # }else {
  #    plot <- NULL
  # }
  


  return(fit_test)
}



train_svm <- function(dat_train) {
  
  ## format training data 
  training_data_elements_svm <- create_ml_args_svm(dat_train)
  formula_svm = training_data_elements_svm[["formula"]]
  
  ## fit
  # fit <- svm(formula_svm,
  #            data = training_data_elements_svm[["data"]], 
  #            gamma = 1, 
  #            cost = 1,
  #            scale = TRUE,
  #            epsolon = 0.1,
  #            type = "eps-regression") 

  fit <- fit(formula_svm, 
            data=training_data_elements_svm[["data"]], 
            model="svm", 
            gamma = 1, 
            C = 1,
            scaled = TRUE,
            epsilon = 0.1,
            kpar = list(sigma = 1),
            type = "eps-svr")

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
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim
) {
  
  ## format data 
  testing_data_elements_svm = create_ml_args_svm(dat_test)
  total_data_elements_svm   = create_ml_args_svm(dat_total)
  
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
  
  return(cf_output)
}

## plot variable importance

# plot_var_importance_svm <- function(dat_train,fit_train, method, fold){

#   training_data_elements_svm <- create_ml_args_svm(dat_train)

#   ## Importance() function requires to take 
#   ## create our own function for input of importance()
#   # fit.pred =function(fit,data) {return (predict(fit,data)) }

#   # svm.imp <- Importance(fit_train, 
#   #                       data=training_data_elements_svm[["data"]],
#   #                       PRED = fit.pred, 
#   #                       outindex = 1, 
#   #                       method = "svm")

#   svm.imp <- Importance(fit_train, data=training_data_elements_svm[["data"]])
#   svm.imp$imp                        

#   df <- cbind(variable = names(training_data_elements_svm[["data"]]),
#               value = round(svm.imp$imp, 3)) %>%
#         as.data.frame() %>%
#         slice(-1)


#   highlight_df <- df %>% filter(variable %in% c("pseudo")) 

#   # ## recode the variable names                
#   # df$variable <- fct_recode(df$variable,
#   #                           "Area population" = "area_pop_base",
#   #                           "Total oustanding debt in area" =  "area_debt_total_base", 
#   #                           "Total number of business in area" = "area_business_total_base", 
#   #                           "Area mean montly pc expenditure" = "area_exp_pc_mean_base", 
#   #                           "Area literacy rate (HH heads)" = "area_literate_head_base","Area literacy rate" = "area_literate_base")

#   # highlight_df$variable  <- fct_recode(highlight_df$variable,
#   #                         "Total oustanding debt in area" = "area_debt_total_base",
#   #                         "Area mean montly pc expenditure" = "area_exp_pc_mean_base",
#   #                         "Area literacy rate" = "area_literate_base")


#   df %>% 
#     ggplot(., aes(x = reorder(variable,value), y = value)) + 
#     geom_bar(stat="identity", fill= rainbow(1), alpha=.4) +
#     geom_bar(data = highlight_df, stat="identity", fill= rainbow(1), alpha=.8) +
#     theme_bw()  +
#     coord_flip() +
#     ggtitle(method) +
#     labs(y = "Coefficient",
#        x = "Variable") 

#   ggsave(here("plot", paste0("svm_var_importance", fold, ".png")), width = 6, height = 4.5, dpi = 300)

# }