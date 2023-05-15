#' Summarize estimate_itr output 
#' @param object An object of \code{estimate_itr} class (typically an output of \code{estimate_itr()} function).
#' @param ... Other parameters. 
#' @importFrom stats pnorm
#' @export
summary.itr <- function(object, ...) {
  out         <- list()
  fit_outcome <- object$df$outcome
  algorithm   <- object$df$algorithm
  cv          <- object$cv
  fit         <- object$qoi

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
        upper = gate - qnorm(0.95) * sd,
        lower = gate + qnorm(0.95) * sd
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
        upper = gate + qnorm(0.975) * sd,
        lower = gate - qnorm(0.975) * sd
      ) %>%
      rename(
        estimate = gate,
        std.deviation = sd,
        algorithm = alg,
        group = group
      )
  }

  class(out) <- c("summary.itr", class(out))

  return(out)
}

#' Print
#' @importFrom cli cat_rule
#' @param x An object of \code{summary.itr} class. This is typically an output of \code{summary.itr()} function.
#' @param ... Other parameters. Currently not supported.
#' @export
print.summary.itr <- function(x, ...) {
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
}



#' Summarize test_itr output 
#' @param object An object of \code{test_itr} class (typically an output of \code{test_itr()} function).
#' @param ... Other parameters. 
#' @importFrom stats pnorm
#' @export
summary.test_itr <- function(object, ...) {
  out            <- list()
  consist_tibble <- tibble()
  het_tibble     <- tibble()

  ## -----------------------------------------
  ## hypothesis tests
  ## -----------------------------------------
  if (names(object[1]) == "consist") {

    # parameters for test_itr object
    consist        <- object$consist
    het            <- object$het
    consist_names <- names(consist)
    het_names <- names(het)

    # reformat
    out[["Consistency"]] <- consist %>%
      map(., ~ as_tibble(.)) %>%
      bind_rows() %>%
      mutate(algorithm = consist_names) %>%
      rename(statistic = stat,
            p.value = pval) %>%
      select(algorithm, statistic, p.value)


    out[["Heterogeneity"]] <- het %>%
      map(., ~ as_tibble(.)) %>%
      bind_rows() %>%
      mutate(algorithm = het_names) %>%
      rename(statistic = stat,
            p.value = pval) %>%
      select(algorithm, statistic, p.value)
  } 


  if (names(object[1]) == "consistcv") {
    
    # parameters for test_itr object
    consist <- object$consistcv
    het <- object$hetcv
    consist_names <- names(consist)
    het_names <- names(het)

    # reformat
    out[["Consistency_cv"]] <- consist %>%
      map(., ~ as_tibble(.)) %>%
      bind_rows() %>%
      mutate(algorithm = consist_names) %>%
      rename(statistic = stat,
            p.value = pval) %>%
      select(algorithm, statistic, p.value)

    out[["Heterogeneity_cv"]] <- het %>%
      map(., ~ as_tibble(.)) %>%
      bind_rows() %>%
      mutate(algorithm = het_names) %>%
      rename(statistic = stat,
            p.value = pval) %>%
      select(algorithm, statistic, p.value)
  }

  class(out) <- c("summary.test_itr", class(out))

  return(out)
}

#' Print
#' @importFrom cli cat_rule
#' @param x An object of \code{summary.test_itr} class. This is typically an output of \code{summary.test_itr()} function.
#' @param ... Other parameters. 
#' @export
print.summary.test_itr <- function(x, ...) {

  # Rank Consistency Test
  cli::cat_rule(left = "Rank Consistency Test Results")
  print(as.data.frame(x[["Consistency"]], digits = 2))
  cli::cat_line("")

  # Group Heterogeneity Test
  cli::cat_rule(left = "Group Heterogeneity Test Results")
  print(as.data.frame(x[["Heterogeneity"]], digits = 2))
  cli::cat_line("")
}

