
## boosted tree

run_boost <- function(
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
  fit_train <- train_boost(dat_train)
  

  ## test 
  fit_test <- test_boost(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb, 
    indcv, iter, plim
  )
  
  # plot
  if(plot == TRUE){
    plot <- plot_var_importance_boost(fit_train, "Boosting", iter)
  }else {
     plot <- NULL
  }

  
  return(fit_test)
}



train_boost <- function(dat_train) {
  
  ## format training data 
  training_data_elements_boosted = create_ml_args_boosted(dat_train)
  
  ## train formula
  formula_boosted = training_data_elements_boosted[["formula"]] 

  ## fit
  fit <- gbm::gbm(formula_boosted, data = training_data_elements_boosted[["data"]],
                    distribution = "gaussian",
                    n.trees = 5000,
                    interaction.depth = 4)

  return(fit)

}

test_boost <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim
) {
  
  ## format data 
  testing_data_elements_boosted = create_ml_args_boosted(dat_test)
  total_data_elements_boosted   = create_ml_args_boosted(dat_total)
  
  ## predict 
  
  Y0t_total=predict(fit_train, as.data.frame(total_data_elements_boosted[["data0t"]]))
  Y1t_total=predict(fit_train, as.data.frame(total_data_elements_boosted[["data1t"]]))

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

plot_var_importance_boost <- function(fit_train, method, fold){
  df <- summary(fit_train) %>% as.data.frame() %>% {{temp <<-.}} %>%
    dplyr::mutate(variable = rownames(temp)) %>%
    rename(value = rel.inf)

  highlight_df <- df[c("pseudo"),]

  # ## recode the variable names                
  # df$variable <- fct_recode(df$variable,
  #                           "Area population" = "area_pop_base",
  #                           "Total oustanding debt in area" =  "area_debt_total_base", 
  #                           "Total number of business in area" = "area_business_total_base", 
  #                           "Area mean montly pc expenditure" = "area_exp_pc_mean_base", 
  #                           "Area literacy rate (HH heads)" = "area_literate_head_base","Area literacy rate" = "area_literate_base")

  #   highlight_df$variable  <- fct_recode(highlight_df$variable,
  #                           "Total oustanding debt in area" = "area_debt_total_base",
  #                           "Area mean montly pc expenditure" = "area_exp_pc_mean_base",
  #                           "Area literacy rate" = "area_literate_base")

  df  %>% 
    ggplot(., aes(x = reorder(variable,value), y = value)) + 
    geom_bar(stat="identity", fill= rainbow(1), alpha=.4) +
    geom_bar(data = highlight_df, stat="identity", fill= rainbow(1), alpha=.8) +
    theme_bw()  +
    coord_flip() +
    ggtitle(method) +
    labs(y = "Coefficient",
       x = "Variable") 


  ggsave(here("plot", paste0("boosting_var_importance", fold, ".png")), width = 6, height = 4.5, dpi = 300)

}


  
