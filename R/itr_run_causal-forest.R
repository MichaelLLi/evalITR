

## causal-forest 

run_causal_forest <- function(
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
  fit_train <- train_causal_forest(dat_train)

  ## test 
  fit_test <- test_causal_forest(
    fit_train, dat_test, dat_total, params$n_df, params$n_tb, 
    indcv, iter, plim
  )
  
  ## plot
  if(plot == TRUE){
    plot <- plot_var_importance_cf(dat_train, fit_train, "Causal Forest", iter)
  }else {
     plot <- NULL
  }
  

  return(fit_test)
}



train_causal_forest <- function(dat_train) {
  
  ## format training data 
  training_data_elements_cf <- create_ml_args_causalforest(dat_train)
  
  ## fit
  fit <- grf::causal_forest(
    training_data_elements_cf[["X_expand"]],
    training_data_elements_cf[["Y"]],
    training_data_elements_cf[["Treat"]],
    ci.group.size = 1,
    sample.fraction = 0.2,
    num.trees = 2000
  )
  return(fit)
}

test_causal_forest <- function(
  fit_train, dat_test, dat_total, n_df, n_tb, indcv, iter, plim
) {
  
  ## format data 
  testing_data_elements_cf <- create_ml_args_causalforest(dat_test)
  total_data_elements_cf   <- create_ml_args_causalforest(dat_total)
  
  ## predict 
  tau_total <- predict(
    fit_train,
    total_data_elements_cf[["X_expand"]]
  )$predictions + runif(n_df,-1e-6,1e-6)
  
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

plot_var_importance_cf <- function(dat_train, fit_train, method, fold){

  ## get covariate names
  cov_names <- dat_train[["X"]] %>% names()

  ## get var importance df
  df <- variable_importance(fit_train) %>% as.data.frame() %>%
    dplyr::mutate(variable = cov_names) %>%
    rename(value = "V1")

  highlight_df <- df %>% 
                  filter(variable %in% c("pseudo")) 

  # ## recode the variable names                
  # df$variable <- fct_recode(df$variable,
  #                           "Area population" = "area_pop_base",
  #                           "Total oustanding debt in area" =  "area_debt_total_base", 
  #                           "Total number of business in area" = "area_business_total_base", 
  #                           "Area mean montly pc expenditure" = "area_exp_pc_mean_base", 
  #                           "Area literacy rate (HH heads)" = "area_literate_head_base","Area literacy rate" = "area_literate_base")

  # highlight_df$variable  <- fct_recode(highlight_df$variable,
  #                         "Total oustanding debt in area" = "area_debt_total_base",
  #                         "Area mean montly pc expenditure" = "area_exp_pc_mean_base",
  #                         "Area literacy rate" = "area_literate_base")

  df %>% 
    ggplot(., aes(x = reorder(variable,value), y = value)) + 
    geom_bar(stat="identity", fill= rainbow(1), alpha=.4) +
    geom_bar(data = highlight_df, stat="identity", fill= rainbow(1), alpha=.8) +
    theme_bw()  +
    coord_flip() +
    ggtitle(method) +
    labs(y = "Coefficient",
       x = "Variable") 

  ggsave(here("plot", paste0("cf_var_importance", fold, ".png")), width = 6, height = 4.5, dpi = 300)

}