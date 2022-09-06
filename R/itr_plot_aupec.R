#' Plot the AUPEC curve 
#' @import ggplot2
#' @import ggthemes
#' @importFrom stats sd
#' @importFrom rlang .data
#' @param fit A list that contains the following items:
#' * `aupec`: The estimated Area Under Prescription Evaluation Curve
#' * `sd`: The estimated standard deviation of AUPEC
#' * `aupec_cv`: The estimated Area Under Prescription Evaluation Curve using cross-validation
#' @param data The dataset.
#' @param treatment A vector of the unit-level binary treatment receipt variable for each sample.
#' @param outcome A vector of the outcome variable of interest for each sample.
#' @param algorithms A vector of machine learning algorithms. 
#' @export plot_aupec 
plot_aupec <- function(fit, data, treatment, outcome, algorithms){

graphLabels <- data.frame(type = algorithms,
                          Pval = bind_rows(map(fit[[1]]$AUPEC, ~.x$aupec_cv)) %>% 
                            mutate(Pval = paste0("AUPEC = ", round(aupec, 2), 
                                                 " (s.e. = ", round(sd, 2), ")")) %>% 
                            pull(Pval))

Tcv = data %>% pull(treatment) %>% as.numeric()
Ycv = data %>% pull(outcome) %>% as.numeric()

bind_rows(map(fit[[1]]$AUPEC, ~.x$aupec_cv)) %>% 
  mutate(type = algorithms) %>%
  inner_join(bind_rows(
    map(fit[[1]]$AUPEC, ~.x$outputdf)),
    by = "type"
  ) %>%
  mutate(AUPECmin = aupec.y - 1.96*sd,
         AUPECmax = aupec.y + 1.96*sd) -> data


data %>% 
  ggplot(aes(x=fraction,y=aupec.y,group=type)) + 
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
      intercept = sum(Ycv*(1-Tcv))/sum(1-Tcv), slope = sum(Ycv*Tcv)/sum(Tcv)-sum(Ycv*(1-Tcv))/sum(1-Tcv),size=0.5) +
    geom_text(
      data = graphLabels, aes(x = 0.57, y = max(data$AUPECmax, na.rm = TRUE)+0.35, label = Pval),size=3) +
    theme(text = element_text(size=13.5),
          axis.text = element_text(size=10),
          strip.text = element_text(size = 13.5)) -> out

return(out)
}