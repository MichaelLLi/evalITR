#' Summarize run_itr output
#' @param object An object of \code{run_itr} class
#' @param m A numeric value indicates which element to focus on from the vector of \code{outcomes}. The default is the first outcome in the vector \code{outcomes}.
#' @param ... Other parameters. Currently not supported
#' @importFrom stats pnorm
#' @export 
summary.itr <- function(object, m = 1, ...){
    
    out <- list()

    out[["PAPE"]] <- object$qoi[[m]]$PAPE %>%
        map(.,~as_tibble(.)) %>%
        bind_rows() %>%
        mutate(
          statistic = pape/sd,
          p.value = 2*pnorm(abs(pape/sd), lower.tail = FALSE)) %>%
        rename(
          estimate = pape,
          std.deviation = sd,
          algorithm = alg)

    out[["PAPEp"]] <- object$qoi[[m]]$PAPEp %>%
        map(.,~as_tibble(.)) %>%
        bind_rows() %>%
        mutate(
          statistic = papep/sd,
          p.value = 2*pnorm(abs(papep/sd), lower.tail = FALSE)) %>%
        rename(
          estimate = papep,
          std.deviation = sd,
          algorithm = alg)

    out[["PAPDp"]] <- object$qoi[[m]]$PAPDp %>%
        map(.,~as_tibble(.)) %>%
        bind_rows() %>%
        mutate(
          statistic = papd/sd,
          p.value = 2*pnorm(abs(papd/sd), lower.tail = FALSE)) %>%
        rename(
          estimate = papd,
          std.deviation = sd,
          algorithm = alg)

    temp <- object$qoi[[m]]$AUPEC 

    out[["AUPEC"]] <- temp %>% 
        map(., ~.x$aupec_cv) %>%
        bind_rows() %>%
        mutate(
            algorithm = temp %>% map(., ~.x$outputdf$type %>% unique) %>% unlist) %>%
        mutate(
          statistic = aupec/sd,
          p.value = 2*pnorm(abs(aupec/sd), lower.tail = FALSE)) %>%
        rename(
          estimate = aupec,
          std.deviation = sd)

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
}
