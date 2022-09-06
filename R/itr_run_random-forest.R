

## random forest

run_random_forest <- function(
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
  fit_train <- train_random_forest(dat_train)
  
  ## test 
  fit_test <- test_random_forest(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb, 
    indcv, iter, plim
  )

  # plot
  # if(plot == TRUE){
  #   plot <- plot_var_importance_rf(fit_train, "RandomForest", iter)
  # }else {
  #    plot <- NULL
  # }
  
  
  return(fit_test)
}



train_random_forest <- function(dat_train) {
  
  ## format training data 
  training_data_elements_rf = create_ml_args_rf(dat_train)
  
  ## train formula
  formula_rf = training_data_elements_rf[["formula"]] 

  ## fit
  fit <- randomForest::randomForest(formula_rf, data = training_data_elements_rf[["data"]], ntree = 500)

  return(fit)
}

#'@importFrom stats predict runif
test_random_forest <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim
) {
  
  ## format data 
  testing_data_elements_rf = create_ml_args_rf(dat_test)
  total_data_elements_rf   = create_ml_args_rf(dat_total)
  
  ## predict 
  
  Y0t_total = predict(fit_train, newdata=total_data_elements_rf[["data0t"]])
  Y1t_total = predict(fit_train, newdata=total_data_elements_rf[["data1t"]])

  tau_total=Y1t_total - Y0t_total + runif(n_df,-1e-6,1e-6)


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


## plot varaible importance

# plot_var_importance_rf <- function(fit_train, method, fold){

#   df <- importance(fit_train) %>% as.data.frame() %>% {{temp <<-.}} %>%
#     dplyr::mutate(variable = rownames(temp)) %>%
#     rename(value = IncNodePurity)

#   highlight_df <- df[c("pseudo"),]

#   # ## recode the variable names                
#   # df$variable <- fct_recode(df$variable,
#   #                           "Area population" = "area_pop_base",
#   #                           "Total oustanding debt in area" =  "area_debt_total_base", 
#   #                           "Total number of business in area" = "area_business_total_base", 
#   #                           "Area mean montly pc expenditure" = "area_exp_pc_mean_base", 
#   #                           "Area literacy rate (HH heads)" = "area_literate_head_base","Area literacy rate" = "area_literate_base")

#   #   highlight_df$variable  <- fct_recode(highlight_df$variable,
#   #                           "Total oustanding debt in area" = "area_debt_total_base",
#   #                           "Area mean montly pc expenditure" = "area_exp_pc_mean_base",
#   #                           "Area literacy rate" = "area_literate_base")

#   df %>% 
#     ggplot(., aes(x = reorder(variable,value), y = value)) + 
#     geom_bar(stat="identity", fill= rainbow(1), alpha=.4) +
#     geom_bar(data = highlight_df, stat="identity", fill= rainbow(1), alpha=.8) +
#     theme_bw()  +
#     coord_flip() +
#     ggtitle(method) +
#     labs(y = "Coefficient",
#        x = "Variable") 

       

#   ggsave(here("plot", paste0("rf_var_importance", fold, ".png")), width = 6, height = 4.5, dpi = 300)

# }


  
