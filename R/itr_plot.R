#' Plot the AUPEC curve 
#' @import ggplot2
#' @import ggthemes
#' @importFrom stats sd
#' @importFrom rlang .data
#' @param x An object of \code{evaluate_itr()} class. This is typically an output of \code{evaluate_itr()} function. 
#' @param ... Further arguments passed to the function.
#' @return A plot of ggplot2 object.
#' @export 
plot.itr <- function(x, ...){

# parameters
estimate = x
estimate_algs = estimate$out_algs
estimate_user = estimate$out_user
data_algs = tibble()
data_user = tibble()

# fit = estimate$qoi
# cv = estimate$cv
# user_itr = ifelse(is.null(estimate$user_itr), FALSE, estimate$user_itr)

# -----------------------------------------
# estimate ITR from ML algorithms 
# -----------------------------------------

if(length(estimate_algs) != 0){

  # parameters
  fit = estimate_algs$qoi
  cv = estimate_algs$cv
    
  # format output under cross validation -----------------------------------------

  if(cv == TRUE){

    # parameters
    outcome = estimate_algs$df$outcome
    data = estimate_algs$df$data
    algorithms = estimate_algs$df$algorithms
    treatment = estimate_algs$df$treatment

    graphLabels <- data.frame(
      type = algorithms,
      Pval = map(
        fit$AUPEC, ~.x$aupec_cv) %>%
        bind_rows() %>%
        mutate(Pval = paste0("AUPEC = ", round(aupec, 2), " (s.e. = ", round(sd, 2), ")")) %>% pull(Pval))

    Tcv = data %>% pull(treatment) %>% as.numeric()
    Ycv = data %>% pull(outcome) %>% as.numeric()

    bind_rows(map(fit$AUPEC, ~.x$aupec_cv)) %>% 
      mutate(type = algorithms) %>%
      inner_join(bind_rows(
        map(fit$AUPEC, ~.x$outputdf)),
        by = "type"
      ) %>%
      mutate(AUPECmin = aupec.y - 1.96*sd,
            AUPECmax = aupec.y + 1.96*sd) %>%
      rename(aupec = aupec.y) -> data_algs
    
  }

  # format output under sample splitting -----------------------------------------
  if(cv == FALSE){
    
    # parameters
    data = estimate_algs$df$data
    algorithms = estimate_algs$df$algorithms

    graphLabels <- data.frame(
      type = algorithms,
      Pval = map(
        fit$AUPEC, ~.x[c('aupec', 'sd')]) %>% 
        bind_rows() %>% 
        mutate(Pval = paste0("AUPEC = ", round(aupec, 2), " (s.e. = ", round(sd, 2), ")")) %>% pull(Pval))

    Tcv = estimate_algs$estimates[['Tcv']] %>% as.numeric()
    Ycv = estimate_algs$estimates[['Ycv']] %>% as.numeric()
    
    map(fit$AUPEC, ~.x) %>% 
      bind_rows() %>%
      mutate(
            aupec = vec + mean(Ycv),
            fraction = rep(seq(1,length(Ycv))/length(Ycv), length(algorithms)),
            type = lapply(algorithms, function(x)rep(x,length(Ycv))) %>% unlist) %>% 
      mutate(AUPECmin = aupec - 1.96*sd,
          AUPECmax = aupec + 1.96*sd)  -> data_algs
  }
} 

# -----------------------------------------
# get ITR from the user-defined function
# -----------------------------------------
if(length(estimate_user) != 0){
  
   # parameters
  fit = estimate_user$qoi
  cv = estimate_user$cv

  Tcv = estimate_user$estimates[['Tcv']] %>% as.numeric()
  Ycv = estimate_user$estimates[['Ycv']] %>% as.numeric()

  graphLabels <- data.frame(
    type = "user-defined ITR",
    Pval = map(
      fit$AUPEC, ~.x[c('aupec', 'sd')]) %>% 
      bind_rows() %>% 
      mutate(Pval = paste0("AUPEC = ", round(aupec, 2), " (s.e. = ", round(sd, 2), ")")) %>% pull(Pval))

  fit$AUPEC %>% 
    bind_rows() %>%
    mutate(
      aupec = vec + mean(Ycv),
      fraction = rep(seq(1,length(Ycv))/length(Ycv),1),
      type = lapply("user-defined ITR", function(x)rep(x,length(Ycv))) %>% unlist) %>% 
    mutate(AUPECmin = aupec - 1.96*sd,
      AUPECmax = aupec + 1.96*sd)  -> data_user   
}

# dataframe for plotting
data <- bind_rows(data_algs, data_user) 

# plot
ggplot(data, aes(x=fraction,y=aupec,group=type)) + 
  geom_line(alpha=0.5,colour="red") + 
  scale_colour_few("Dark")+
  xlab("Maximum Proportion Treated")+
  ylab("AUPEC")+
  facet_wrap(~type)+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(
    limits = c(min(data$AUPECmin, na.rm = TRUE)-0.5, max(data$AUPECmax, na.rm = TRUE)+ 0.5))+ 
  theme_few()+ 
  geom_ribbon(
    aes(ymin=AUPECmin, ymax=AUPECmax),fill="tomato1",alpha=0.2) +
  geom_abline(
    intercept = sum(Ycv*(1-Tcv))/sum(1-Tcv), slope = sum(Ycv*Tcv)/sum(Tcv)-sum(Ycv*(1-Tcv))/sum(1-Tcv),linewidth=0.5) +
  geom_text(
    data = graphLabels, aes(x = 0.57, y = max(data$AUPECmax, na.rm = TRUE)+0.35, label = Pval),size=3) +
  theme(text = element_text(size=13.5),
        axis.text = element_text(size=10),
        strip.text = element_text(size = 13.5)) -> out

  return(out)
}


#' Plot the GATE estimate
#' @import ggplot2
#' @import ggthemes
#' @importFrom stats sd
#' @importFrom rlang .data
#' @param x An table object. This is typically an output of \code{evaluate_itr()} function. 
#' @param ... Further arguments passed to the function.
#' @return A plot of ggplot2 object.
#' @export 
plot_estimate <- function(x, type, ...){

# parameters
estimate = x

if(type == "GATE"){
  estimate %>%
    ggplot(., aes(
      x = group, y = estimate,
      ymin = lower , ymax = upper, color = algorithm)) +
    ggdist::geom_pointinterval(
      width = 0.5,
      position = position_dodge(0.5),
      interval_size_range = c(0.8, 1.5),
      fatten_point = 2.5) +  
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.background = element_blank()) +
    labs(x = "Group", y = "GATE estimate") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#4e4e4e")
}

if(type %in% c("PAPE", "PAPEp", "PAPDp")){

  estimate %>%
    mutate(
      algorithm = as.factor(algorithm),
    lower = estimate - 1.96 * std.deviation,
    upper = estimate + 1.96 * std.deviation) %>%
    ggplot(., aes(
      x = algorithm, y = estimate,
      ymin = lower , ymax = upper, color = algorithm)) +
    ggdist::geom_pointinterval(
      width = 0.5,
      position = position_dodge(0.5),
      interval_size_range = c(0.8, 1.5),
      fatten_point = 2.5) +  
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.background = element_blank()) +
    labs(x = "Algorithm", y = type) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#4e4e4e")

  }

}
