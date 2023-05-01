#' Summarize estimate_itr output
#' @param object An object of \code{estimate_itr} class
#' @param outcome Outcome variable. Can be different from the input of \code{estimate_itr}
#' @param ... Other parameters. Currently not supported
#' @importFrom stats pnorm
#' @export
summary.itr <- function(object,
                        outcome = TRUE,
                        # test = TRUE,
                        ...) {

  # Empty list thats stores outputs
  out         <- list()

  # test objects
  if (names(test[1]) == "consist") {
    # parameters for test_itr object
    consist <- object$consist
    het <- object$het
    consist_tibble <- tibble()
    het_tibble <- tibble()

    # reformat
    consist_names <- names(consist)

    out[["Consistency"]] <- consist %>%
      map(., ~ as_tibble(.)) %>%
      bind_rows() %>%
      mutate("Algorithm" = consist_names) %>%
      rename("Test Statistic" = stat,
             "p-value" = pval) %>%
      select("Algorithm", "Test Statistic", "p-value")

    het_names <- names(het)

    out[["Heterogeneity"]] <- het %>%
      map(., ~ as_tibble(.)) %>%
      bind_rows() %>%
      mutate("Algorithm" = het_names) %>%
      rename("Test Statistic" = stat,
             "p-value" = pval) %>%
      select("Algorithm", "Test Statistic", "p-value")

  # test objects
  } else if (names(test[1]) == "consistcv") {
    consist <- object$consistcv
    het <- object$hetcv
    consist_tibble <- tibble()
    het_tibble <- tibble()

    # reformat
    consist_names <- names(consist)

    out[["Consistency_cv"]] <- consist %>%
      map(., ~ as_tibble(.)) %>%
      bind_rows() %>%
      mutate("Algorithm" = consist_names) %>%
      rename("Test Statistic" = stat,
             "p-value" = pval) %>%
      select("Algorithm", "Test Statistic", "p-value")

    het_names <- names(het)

    out[["Heterogeneity_cv"]] <- het %>%
      map(., ~ as_tibble(.)) %>%
      bind_rows() %>%
      mutate("Algorithm" = het_names) %>%
      rename("Test Statistic" = stat,
             "p-value" = pval) %>%
      select("Algorithm", "Test Statistic", "p-value")

  # estimate_itr objects
  } else {
    fit_outcome <- object$df$outcome
    algorithm   <- object$df$algorithm
    cv          <- object$cv

    # reformat
    if(outcome != TRUE){
      # plot user selected outcome
      m = which(fit_outcome == outcome)
      fit <- object$qoi[[m]]
    }else {
      m = 1 # plot the first outcome
      fit <- object$qoi[[1]]
    }

    ## -----------------------------------------
    ## compute quantities under sample splitting
    ## -----------------------------------------
    if (cv == FALSE) {
      out[["PAPE"]] <- fit$PAPE %>%
        map(., ~ as_tibble(.)) %>%
        bind_rows() %>%
        mutate(
          statistic = pape / sd,
          p.value = 2 * pnorm(abs(pape / sd), lower.tail = FALSE)
        ) %>%
        rename(
          estimate = pape,
          std.deviation = sd,
          algorithm = alg
        )

      out[["PAPEp"]] <- fit$PAPEp %>%
        map(., ~ as_tibble(.)) %>%
        bind_rows() %>%
        mutate(
          statistic = pape / sd, # names is pape but not papep
          p.value = 2 * pnorm(abs(pape / sd), lower.tail = FALSE)
        ) %>%
        rename(
          estimate = pape,
          std.deviation = sd,
          algorithm = alg
        )

      # not print out papd if only one alg is selected
      if (length(fit$PAPDp) == 0) {
        out[["PAPDp"]] <- NULL
      } else {
        out[["PAPDp"]] <- fit$PAPDp %>%
          map(., ~ as_tibble(.)) %>%
          bind_rows() %>%
          mutate(
            statistic = papd / sd,
            p.value = 2 * pnorm(abs(papd / sd), lower.tail = FALSE)
          ) %>%
          rename(
            estimate = papd,
            std.deviation = sd,
            algorithm = alg
          )
      }

      temp <- fit$AUPEC

      out[["AUPEC"]] <- temp %>%
        map(., ~ .x[c("aupec", "sd")]) %>%
        bind_rows() %>%
        mutate(algorithm = algorithm) %>%
        mutate(
          statistic = aupec / sd,
          p.value = 2 * pnorm(abs(aupec / sd), lower.tail = FALSE)
        ) %>%
        rename(
          estimate = aupec,
          std.deviation = sd
        )

      out[["GATE"]] <- fit$GATE %>%
        map(., ~ as_tibble(.)) %>%
        bind_rows() %>%
        mutate(
          statistic = gate / sd,
          p.value = 2 * pnorm(abs(gate / sd), lower.tail = FALSE),
          upper = c(mean(gate) - qnorm(0.95) * sd),
          lower = c(mean(gate) + qnorm(0.95) * sd)
        ) %>%
        rename(
          estimate = gate,
          std.deviation = sd,
          algorithm = alg,
          group = group
        )
    }

    if (cv == TRUE) {
      ## -----------------------------------------
      ## compute quantities under cross-validation
      ## -----------------------------------------
      out[["PAPE"]] <- fit$PAPE %>%
        map(., ~ as_tibble(.)) %>%
        bind_rows() %>%
        mutate(
          statistic = pape / sd,
          p.value = 2 * pnorm(abs(pape / sd), lower.tail = FALSE)
        ) %>%
        rename(
          estimate = pape,
          std.deviation = sd,
          algorithm = alg
        )

      out[["PAPEp"]] <- fit$PAPEp %>%
        map(., ~ as_tibble(.)) %>%
        bind_rows() %>%
        mutate(
          statistic = papep / sd,
          p.value = 2 * pnorm(abs(papep / sd), lower.tail = FALSE)
        ) %>%
        rename(
          estimate = papep,
          std.deviation = sd,
          algorithm = alg
        )

      # not print out papd if only one alg is selected
      if (length(fit$PAPDp) == 0) {
        out[["PAPDp"]] <- NULL
      } else {
        out[["PAPDp"]] <- fit$PAPDp %>%
          map(., ~ as_tibble(.)) %>%
          bind_rows() %>%
          mutate(
            statistic = papd / sd,
            p.value = 2 * pnorm(abs(papd / sd), lower.tail = FALSE)
          ) %>%
          rename(
            estimate = papd,
            std.deviation = sd,
            algorithm = alg
          )
      }

      temp <- fit$AUPEC

      out[["AUPEC"]] <- temp %>%
        map(., ~ .x$aupec_cv) %>%
        bind_rows() %>%
        mutate(
          algorithm = temp %>% map(., ~ .x$outputdf$type %>% unique()) %>% unlist()
        ) %>%
        mutate(
          statistic = aupec / sd,
          p.value = 2 * pnorm(abs(aupec / sd), lower.tail = FALSE)
        ) %>%
        rename(
          estimate = aupec,
          std.deviation = sd
        )

      out[["GATE"]] <- fit$GATE %>%
        map(., ~ as_tibble(.)) %>%
        bind_rows() %>%
        mutate(
          statistic = gate / sd,
          p.value = 2 * pnorm(abs(gate / sd), lower.tail = FALSE),
          upper = c(mean(gate) - qnorm(0.95) * sd),
          lower = c(mean(gate) + qnorm(0.95) * sd)
        ) %>%
        rename(
          estimate = gate,
          std.deviation = sd,
          algorithm = alg,
          group = group
        )
    }
  }

  class(out) <- c("summary.itr", class(out))
  return(out)
}

#' Print summary.itr
#' @importFrom cli cat_rule
#' @param x An object of \code{summary.itr} class. This is typically an output of \code{summary.itr()} function.
#' @param ... Other parameters. Currently not supported.
#' @export
print.summary.itr <- function(x,
                              # test,
                              ...) {

  # if(test == TRUE){
  #   # Tests
  #   cli::cat_rule(left = "Rank Consistency Test Results")
  #   print(as.data.frame(consist_tibble), n_extra = 200)
  #   cli::cat_line("")
  #
  #   cli::cat_rule(left = "Within Group Heterogeneity Test Results")
  #   print(as.data.frame(het_tibble), n_extra = 200)
  #   cli::cat_line("")

  # PAPE
  cli::cat_rule(left = "PAPE")
  print(as.data.frame(x[["PAPE"]]), digits = 2)
  cli::cat_line("")

  # PAPE
  cli::cat_rule(left = "PAPEp")
  print(as.data.frame(x[["PAPEp"]]), digits = 2)
  cli::cat_line("")

  # PAPE
  cli::cat_rule(left = "PAPDp")
  print(as.data.frame(x[["PAPDp"]]), digits = 2)
  cli::cat_line("")

  # PAPE
  cli::cat_rule(left = "AUPEC")
  print(as.data.frame(x[["AUPEC"]]), digits = 2)
  cli::cat_line("")

  # GATE
  cli::cat_rule(left = "GATE")
  print(as.data.frame(x[["GATE"]]), digits = 2)
  cli::cat_line("")

      # Tests
  cli::cat_rule(left = "Rank Consistency Test Results")
  print(as.data.frame(x[["Consistency"]], digits = 2))
  cli::cat_line("")

  cli::cat_rule(left = "Group Heterogeneity Test Results")
  print(as.data.frame(x[["Heterogeneity"]], digits = 2))
  cli::cat_line("")

}
