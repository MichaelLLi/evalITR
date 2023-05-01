#' Summarize estimate_itr output
#' @param object An object of \code{estimate_itr} class
#' @param outcome Outcome variable. Can be different from the input of \code{estimate_itr}
#' @param ... Other parameters. Currently not supported
#' @importFrom stats pnorm
#' @export
summary.itr <- function(object,
                        # outcome = TRUE,
                        # test = TRUE,
                        ...) {

  # out         <- list()
  # fit_outcome <- object$df$outcome
  # algorithm   <- object$df$algorithm
  # cv          <- object$cv

  # if(test == TRUE){
    # parameters for test_itr object
    consist <- object$consist
    het <- object$het
    consist_tibble <- tibble()
    het_tibble <- tibble()
  # }

  # if(outcome != TRUE){
  #   # plot user selected outcome
  #   m = which(fit_outcome == outcome)
  #   fit <- object$qoi[[m]]
  # }else {
  #   m = 1 # plot the first outcome
  #   fit <- object$qoi[[1]]
  # }
  #
  # ## -----------------------------------------
  # ## compute quantities under sample splitting
  # ## -----------------------------------------
  # if (cv == FALSE) {
  #   out[["PAPE"]] <- fit$PAPE %>%
  #     map(., ~ as_tibble(.)) %>%
  #     bind_rows() %>%
  #     mutate(
  #       statistic = pape / sd,
  #       p.value = 2 * pnorm(abs(pape / sd), lower.tail = FALSE)
  #     ) %>%
  #     rename(
  #       estimate = pape,
  #       std.deviation = sd,
  #       algorithm = alg
  #     )
  #
  #   out[["PAPEp"]] <- fit$PAPEp %>%
  #     map(., ~ as_tibble(.)) %>%
  #     bind_rows() %>%
  #     mutate(
  #       statistic = pape / sd, # names is pape but not papep
  #       p.value = 2 * pnorm(abs(pape / sd), lower.tail = FALSE)
  #     ) %>%
  #     rename(
  #       estimate = pape,
  #       std.deviation = sd,
  #       algorithm = alg
  #     )
  #
  #   # not print out papd if only one alg is selected
  #   if (length(fit$PAPDp) == 0) {
  #     out[["PAPDp"]] <- NULL
  #   } else {
  #     out[["PAPDp"]] <- fit$PAPDp %>%
  #       map(., ~ as_tibble(.)) %>%
  #       bind_rows() %>%
  #       mutate(
  #         statistic = papd / sd,
  #         p.value = 2 * pnorm(abs(papd / sd), lower.tail = FALSE)
  #       ) %>%
  #       rename(
  #         estimate = papd,
  #         std.deviation = sd,
  #         algorithm = alg
  #       )
  #   }
  #
  #   temp <- fit$AUPEC
  #
  #   out[["AUPEC"]] <- temp %>%
  #     map(., ~ .x[c("aupec", "sd")]) %>%
  #     bind_rows() %>%
  #     mutate(algorithm = algorithm) %>%
  #     mutate(
  #       statistic = aupec / sd,
  #       p.value = 2 * pnorm(abs(aupec / sd), lower.tail = FALSE)
  #     ) %>%
  #     rename(
  #       estimate = aupec,
  #       std.deviation = sd
  #     )
  #
  #   out[["GATE"]] <- fit$GATE %>%
  #     map(., ~ as_tibble(.)) %>%
  #     bind_rows() %>%
  #     mutate(
  #       statistic = gate / sd,
  #       p.value = 2 * pnorm(abs(gate / sd), lower.tail = FALSE),
  #       upper = c(mean(gate) - qnorm(0.95) * sd),
  #       lower = c(mean(gate) + qnorm(0.95) * sd)
  #     ) %>%
  #     rename(
  #       estimate = gate,
  #       std.deviation = sd,
  #       algorithm = alg,
  #       group = group
  #     )
  # }
  #
  # if (cv == TRUE) {
  #   ## -----------------------------------------
  #   ## compute quantities under cross-validation
  #   ## -----------------------------------------
  #   out[["PAPE"]] <- fit$PAPE %>%
  #     map(., ~ as_tibble(.)) %>%
  #     bind_rows() %>%
  #     mutate(
  #       statistic = pape / sd,
  #       p.value = 2 * pnorm(abs(pape / sd), lower.tail = FALSE)
  #     ) %>%
  #     rename(
  #       estimate = pape,
  #       std.deviation = sd,
  #       algorithm = alg
  #     )
  #
  #   out[["PAPEp"]] <- fit$PAPEp %>%
  #     map(., ~ as_tibble(.)) %>%
  #     bind_rows() %>%
  #     mutate(
  #       statistic = papep / sd,
  #       p.value = 2 * pnorm(abs(papep / sd), lower.tail = FALSE)
  #     ) %>%
  #     rename(
  #       estimate = papep,
  #       std.deviation = sd,
  #       algorithm = alg
  #     )
  #
  #   # not print out papd if only one alg is selected
  #   if (length(fit$PAPDp) == 0) {
  #     out[["PAPDp"]] <- NULL
  #   } else {
  #     out[["PAPDp"]] <- fit$PAPDp %>%
  #       map(., ~ as_tibble(.)) %>%
  #       bind_rows() %>%
  #       mutate(
  #         statistic = papd / sd,
  #         p.value = 2 * pnorm(abs(papd / sd), lower.tail = FALSE)
  #       ) %>%
  #       rename(
  #         estimate = papd,
  #         std.deviation = sd,
  #         algorithm = alg
  #       )
  #   }
  #
  #   temp <- fit$AUPEC
  #
  #   out[["AUPEC"]] <- temp %>%
  #     map(., ~ .x$aupec_cv) %>%
  #     bind_rows() %>%
  #     mutate(
  #       algorithm = temp %>% map(., ~ .x$outputdf$type %>% unique()) %>% unlist()
  #     ) %>%
  #     mutate(
  #       statistic = aupec / sd,
  #       p.value = 2 * pnorm(abs(aupec / sd), lower.tail = FALSE)
  #     ) %>%
  #     rename(
  #       estimate = aupec,
  #       std.deviation = sd
  #     )
  #
  #   out[["GATE"]] <- fit$GATE %>%
  #     map(., ~ as_tibble(.)) %>%
  #     bind_rows() %>%
  #     mutate(
  #       statistic = gate / sd,
  #       p.value = 2 * pnorm(abs(gate / sd), lower.tail = FALSE),
  #       upper = c(mean(gate) - qnorm(0.95) * sd),
  #       lower = c(mean(gate) + qnorm(0.95) * sd)
  #     ) %>%
  #     rename(
  #       estimate = gate,
  #       std.deviation = sd,
  #       algorithm = alg,
  #       group = group
  #     )
  #
  # }
  # summarize test outputs
  # out[["consist.test"]] <-
    x[["consist_tibble"]] <- as.tibble(consist) %>% #add to out as a element of the list
    t()
  # colnames(consist_tibble) <- c("Test Statistic", "p-value")
#
#   out[["het.test"]] <-
    x[["het_tibble"]] <- as.tibble(het) %>%
    t()
  # colnames(het_tibble) <- c("Test Statistic", "p-value")

  # final output

  # class(out) <- c("summary.itr", class(out))
  class(x) <- c("summary.itr", class(x))
  # class(consist_tibble) <- c("summary.itr", class(consist_tibble))
  # class(het_tibble) <- c("summary.itr", class(het_tibble))

  # return_list <- c(
  #   # out,
  #   consist_tibble, het_tibble)
  return(x)
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
  #
  # # }
  # if(test == FALSE){
    # # PAPE
    # cli::cat_rule(left = "PAPE")
    # print(as.data.frame(x[["PAPE"]]), digits = 2)
    # cli::cat_line("")
    #
    # # PAPE
    # cli::cat_rule(left = "PAPEp")
    # print(as.data.frame(x[["PAPEp"]]), digits = 2)
    # cli::cat_line("")
    #
    # # PAPE
    # cli::cat_rule(left = "PAPDp")
    # print(as.data.frame(x[["PAPDp"]]), digits = 2)
    # cli::cat_line("")
    #
    # # PAPE
    # cli::cat_rule(left = "AUPEC")
    # print(as.data.frame(x[["AUPEC"]]), digits = 2)
    # cli::cat_line("")
    #
    # # GATE
    # cli::cat_rule(left = "GATE")
    # print(as.data.frame(x[["GATE"]]), digits = 2)
    # cli::cat_line("")

      # Tests
      cli::cat_rule(left = "Rank Consistency Test Results")
      print(as.data.frame(consist_tibble), n_extra = 200)
      cli::cat_line("")

      cli::cat_rule(left = "Within Group Heterogeneity Test Results")
      print(as.data.frame(het_tibble), n_extra = 200)
      cli::cat_line("")

  # }
}

#' #' Summarize test_itr output
#' #' @param object An object of \code{test_itr} class
#' #' @param ... Other parameters. Currently not supported
#' #' @importFrom stats pnorm
#' #' @export
#'
#' summary.test.itr <- function(test, ...) {
#'   consist <- test$consist
#'   het <- test$het
#'   consist_tibble <- tibble()
#'   het_tibble <- tibble()
#'
#'   consist_tibble <- as.tibble(consist) %>% #add to out as a element of the list
#'     t() %>%
#'     as.data.frame()
#'   colnames(consist_tibble) <- c("Test Statistic", "p-value")
#'
#'   het_tibble <- as.tibble(het) %>%
#'     t() %>%
#'     as.data.frame()
#'   colnames(het_tibble) <- c("Test Statistic", "p-value")
#'
#'   class(consist_tibble) <- c("summary.itr", class(consist_tibble))
#'   class(het_tibble) <- c("summary.itr", class(het_tibble))
#'   return(consist_tibble)
#'   return(het_tibble)
#' }

#' #' Print summary.itr
#' #' @importFrom cli cat_rule
#' #' @param x An object of \code{summary.test_itr} class. This is typically an output of \code{summary.test_itr()} function.
#' #' @param ... Other parameters. Currently not supported.
#' #' @export
#' print.summary.test.itr <- function(x, ...) {
#'
#'   # Tests
#'   cli::cat_rule(left = "Rank Consistency Test Results")
#'   print(consist_tibble, digits = 2)
#'   cli::cat_line("")
#'
#'   cli::cat_rule(left = "Within Group Heterogeneity Test Results")
#'   print(het_tibble, digits = 2)
#'   cli::cat_line("")
#' }
