#' Summarize run_itr output
#' @param object An object of \code{run_itr} class.
#' @param type A string indicates the quantity of interests to compute. \code{type} includes PAPE, PAPEp, PAPDp
#' @param m A numeric value indicates which outcome to look at from the vector of \code{outcomes}
#' @param ... Other parameters. Currently not supported
#' @export 
summary.itr <- function(object, m, type, ...){
    if(type == "PAPE"){
    out <- object$qoi[[m]]$PAPE %>%
        map(.,~as_tibble(.)) %>% bind_rows()
    }else if (type == "PAPEp") {
    out <- object$qoi[[m]]$PAPEp %>%
        map(.,~as_tibble(.)) %>%
        bind_rows()
    }else if (type == "PAPDp") {
    out <- object$qoi[[m]]$PAPDp %>%
        map(.,~as_tibble(.)) %>%
        bind_rows()
    }else if (type == "AUPEC") {
    temp <- object$qoi[[m]]$AUPEC 
    out <- temp %>% map(., ~.x$aupec_cv) %>%
        bind_rows() %>%
        mutate(algorithm = temp %>% map(., ~.x$outputdf$type %>% unique) %>% unlist)
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
  cli::cat_rule(left = "Estimates")
  print(as.data.frame(x), digits = 2)
  invisible(x)
  x
}
