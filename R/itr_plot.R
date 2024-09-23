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

#' Plot the AUPEC curve
#' @import ggplot2
#' @import ggthemes
#' @import purrr
#' @importFrom stats sd
#' @param x An object of \code{AUPEC()} class. This is typically an output of \code{AUPEC()} function.
#' @param Y The outcome variable.
#' @param T The treatment variable.
#' @param ... Further arguments passed to the function.
#' @return A plot of ggplot2 object. The plot shows the AUPEC curve across each possible budget point for the dataset. Each step increases the budget by 1/n where n is the number of data points. 
#' @export
plot.aupec <- function(x, Y, T, ...) {

  estimate = x
  
  # format the data
  data <- tibble(
    fraction = seq(1,length(Y))/length(Y),
    aupec = aupeclist$vec + mean(Y),
    sd = aupeclist$sd,
    AUPECmin = aupec - 1.96*aupeclist$sd,
    AUPECmax = aupec + 1.96*aupeclist$sd
  )

  # format the labels
  graphLabels <- data.frame(
    Pval = paste0("AUPEC = ", round(aupeclist$aupec, 2), " (s.e. = ", round(aupeclist$sd, 2), ")"))

  # plot
  ggplot(data, aes(x=fraction,y=aupec)) +
    geom_line(alpha=0.5,colour="red") +
    scale_colour_few("Dark")+
    xlab("Maximum Proportion Treated")+
    ylab("AUPEC")+
    scale_x_continuous(labels=scales::percent)+
    scale_y_continuous(
      limits = c(min(data$AUPECmin, na.rm = TRUE)-0.5, max(data$AUPECmax, na.rm = TRUE)+ 0.5))+
    theme_few()+
    geom_ribbon(
      aes(ymin=AUPECmin, ymax=AUPECmax),fill="tomato1",alpha=0.2) +
    geom_abline(
      intercept = sum(Y*(1-T))/sum(1-T), slope = sum(Y*T)/sum(T)-sum(Y*(1-T))/sum(1-T),linewidth=0.5) +
    geom_text(
      data = graphLabels, aes(x = 0.57, y = max(data$AUPECmax, na.rm = TRUE)+0.35, label = Pval),size=3) +
    theme(text = element_text(size=13.5),
          axis.text = element_text(size=10),
          strip.text = element_text(size = 13.5)) -> plot

  return(plot)
}


#' Plot the AUPEC curve under cross-validation
#' @import ggplot2
#' @import ggthemes
#' @import purrr
#' @importFrom stats sd
#' @param x An object of \code{AUPECcv()} class. This is typically an output of \code{AUPECcv()} function.
#' @param tau A vector of the unit-level continuous score for treatment assignment. 
#' @param tau_cv A matrix where the \code{i}th column is the unit-level continuous score for treatment assignment generated in the \code{i}th fold.
#' @param Y The outcome variable.
#' @param T The treatment variable.
#' @param ind A vector of integers (between 1 and number of folds inclusive) indicating which testing set does each sample belong to.
#' @param ... Further arguments passed to the function.
#' @return A plot of ggplot2 object. The plot shows the AUPEC curve across each possible budget point for the dataset. Each step increases the budget by 1/n where n is the number of data points.
#' @export
plot.aupec_cv <- function(
  x, tau, tau_cv, Y, T, ind, ...) {

  estimate = x
  
  # get cross-validated AUPEC
  get_aupec_cv(
    tau = tau,
    tau_cv = tau_cv,
    Ycv = Y,
    Tcv = T,
    indcv = ind
  ) -> aupec_data

  # format the data
  aupec_data$outputdf %>% as_tibble() %>%
    mutate(
      sd = aupec_data$aupec_cv$sd,
      AUPECmin = aupec - 1.96*sd,
      AUPECmax = aupec + 1.96*sd) -> data

  # format the labels
  graphLabels <- data.frame(
    Pval = paste0("AUPEC = ", round(aupec_data$aupec_cv$aupec, 2), " (s.e. = ", round(aupec_data$aupec_cv$sd, 2), ")"))

  # plot
  data %>%
    ggplot(aes(x=fraction,y=aupec)) +
    geom_line(alpha=0.5,colour="red") +
    scale_colour_few("Dark")+
    xlab("Maximum Proportion Treated")+
    ylab("AUPEC")+
    scale_x_continuous(labels=scales::percent)+
    scale_y_continuous(
      limits = c(min(data$AUPECmin, na.rm = TRUE)-0.5, max(data$AUPECmax, na.rm = TRUE)+ 0.5))+
    theme_few()+
    geom_ribbon(
      aes(ymin=AUPECmin, ymax=AUPECmax),fill="tomato1",alpha=0.2) +
    geom_abline(
      intercept = sum(Y*(1-T))/sum(1-T), slope = sum(Y*T)/sum(T)-sum(Y*(1-T))/sum(1-T),linewidth=0.5) +
    geom_text(
      data = graphLabels, aes(x = 0.57, y = max(data$AUPECmax, na.rm = TRUE)+0.35, label = Pval),size=3) +
    theme(text = element_text(size=13.5),
          axis.text = element_text(size=10),
          strip.text = element_text(size = 13.5)) -> plot

  return(plot)

}

#' Plot the GATE estimate
#' @import ggplot2
#' @import ggthemes
#' @importFrom stats sd
#' @importFrom rlang .data
#' @param x An table object. This is typically an output of \code{evaluate_itr()} function.
#' @param type The metric that you wish to plot. One of GATE, PAPE, PAPEp, or PAPDp.
#' @param ... Further arguments passed to the function.
#' @importFrom ggplot2 .data
#' @return A plot of ggplot2 object.
#' @export
plot_estimate <- function(x, type, ...){

# parameters
estimate = x

if(type == "GATE"){

gate_ggplot <- function(data) {
    ggplot(data, aes(
      x = .data$group, y = .data$estimate,
      ymin = .data$lower , ymax = .data$upper, color = .data$algorithm)) +
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

out = gate_ggplot(estimate)
}

if(type %in% c("PAPE", "PAPEp", "PAPDp")){

  estimate <- estimate %>%
    mutate(
      algorithm = as.factor(algorithm),
      lower = .data$estimate - 1.96 * .data$std.deviation,
      upper = .data$estimate + 1.96 * .data$std.deviation)
  pape_ggplot <- function(data) {
    ggplot(data, aes(
      x = .data$algorithm, y = .data$estimate,
      ymin = .data$lower , ymax = .data$upper, color = .data$algorithm)) +
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

  out = pape_ggplot(estimate)

  }

return(out)

}
