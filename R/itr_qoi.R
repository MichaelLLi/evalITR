#' Compute Quantities of Interest (PAPE, PAPEp, PAPDp, AUPEC, GATE, GATEcv, URATE)
#' @param fit_obj An output object from \code{fit_itr} function.
#' @param algorithms Machine learning algorithms
#' @importFrom rlang .data

compute_qoi <- function(fit_obj, algorithms) {

  ## extract objects
  fit_ml <- fit_obj$fit_ml
  params <- fit_obj$params
  Ycv    <- fit_obj$Ycv
  Tcv    <- fit_obj$Tcv
  indcv  <- fit_obj$indcv
  budget <- fit_obj$budget
  cv     <- fit_obj$params$cv

  ## -----------------------------------------
  ## compute quantities under cross validation
  ## -----------------------------------------
  if (cv == TRUE) {

    ## PAPE and PAPEp
    PAPE <- PAPEp <- vector("list", params$n_alg)
    for (i in seq_len(params$n_alg)) {
      ## make That_cv into matrix
      That_cv_mat <- furrr::future_map(fit_ml[[ algorithms[i] ]], ~.x$That_cv) %>% do.call(cbind, .)
      That_pcv_mat <- furrr::future_map(fit_ml[[ algorithms[i] ]], ~.x$That_pcv) %>% do.call(cbind, .)

      ## compute PAPE
      PAPE[[i]] <- PAPEcv(Tcv, That_cv_mat, Ycv, indcv)

      ## compute PAPEp
      PAPEp[[i]] <- PAPEcv(Tcv, That_pcv_mat, Ycv, indcv, budget)

      ## name
      PAPE[[i]]$alg <-  PAPEp[[i]]$alg <- algorithms[i]
    }


    ## PAPDp
    PAPDp <- list()
    if (params$n_alg > 1) {
      count <- 1

      for (i in 1:(params$n_alg-1)) {
        That_pcv_i <- furrr::future_map(fit_ml[[ algorithms[i] ]], ~.x$That_pcv) %>% do.call(cbind, .)

        for (j in (i+1):params$n_alg) {
          # compare algorithm[i] and algorithm[j]
          That_pcv_j <- furrr::future_map(fit_ml[[ algorithms[j] ]], ~.x$That_pcv) %>% do.call(cbind, .)

          PAPDp[[count]] <- PAPDcv(
            Tcv, That_pcv_i, That_pcv_j, Ycv, indcv, budget
          )

          PAPDp[[count]]$alg <- paste0(algorithms[i], " x ", algorithms[j])
          ## update iterator
          count <- count + 1
        }
      }
    } else {
      cat("Cannot compute PAPDp")
    }


    ##  Compute AUPEC
    aupec <- vector("list", length = length(algorithms))
    for (i in seq_along(algorithms)) {
      tau <- furrr::future_map(fit_ml[[i]], ~.x$tau) %>% do.call(cbind, .)
      tau_cv <- furrr::future_map(fit_ml[[i]], ~.x$tau_cv) %>% do.call(cbind, .)
      That_pcv_mat <- furrr::future_map(fit_ml[[i]], ~.x$That_pcv) %>% do.call(cbind, .)

      aupec[[i]] <- getAupecOutput(
        tau, tau_cv, That_pcv_mat, algorithms[i],
        NFOLDS = params$n_folds, Ycv = Ycv, Tcv = Tcv, indcv = indcv
      )
    }

    ## GATE
    GATE <- vector("list", length = length(algorithms))
    for (i in seq_along(algorithms)) {
      tau <- furrr::future_map(fit_ml[[i]], ~.x$tau) %>% do.call(cbind, .)
      tau_cv <- furrr::future_map(fit_ml[[i]], ~.x$tau_cv) %>% do.call(cbind, .)

      ## Compute GATE
      GATE[[i]] <- GATEcv(Tcv, tau_cv, Ycv, indcv, params$ngates)

      ## indicate algorithm
      GATE[[i]]$alg <- algorithms[i]

      ## indicate group number
      GATE[[i]]$group <- seq_along(GATE[[i]]$gate)
    }

    ## URATE
    URATE <- vector("list", length = length(algorithms))
    for (i in seq_along(algorithms)) {
      tau <- furrr::future_map(fit_ml[[i]], ~.x$tau) %>% do.call(cbind, .)
      tau_cv <- furrr::future_map(fit_ml[[i]], ~.x$tau_cv) %>% do.call(cbind, .)

      ## Compute URATE
      URATE[[i]] <- URATEcv(Tcv, tau_cv, Ycv, indcv)

      ## indicate algorithm
      URATE[[i]]$alg <- algorithms[i]

    }

  }


  ## -----------------------------------------
  ## compute quantities under sample splitting
  ## -----------------------------------------
  if (cv == FALSE) {

    ## PAPE and PAPEp
    PAPE <- PAPEp <- vector("list", params$n_alg)
    for (i in seq_len(params$n_alg)) {

      ## compute PAPE
      PAPE[[i]] <- PAPE(Tcv, fit_ml[[i]][["That_cv"]], Ycv, centered = TRUE)

      ## compute PAPEp: sp does not have papep, check PAPE.R
      PAPEp[[i]] <- PAPE(Tcv, fit_ml[[i]][["That_pcv"]], Ycv, centered = TRUE, budget)

      ## name
      PAPE[[i]]$alg <-  PAPEp[[i]]$alg <- algorithms[i]
    }

    ## PAPDp
    PAPDp <- list()
    if (params$n_alg > 1) {
      count <- 1

      for (i in 1:(params$n_alg-1)) {
        That_pcv_i <- fit_ml[[i]][["That_pcv"]]

        for (j in (i+1):params$n_alg) {
          # compare algorithm[i] and algorithm[j]
          That_pcv_j <- fit_ml[[j]][["That_pcv"]]

          PAPDp[[count]] <- PAPD(
            Tcv, That_pcv_i, That_pcv_j, Ycv, budget, centered = TRUE
          )

          PAPDp[[count]]$alg <- paste0(algorithms[i], " x ", algorithms[j])
          ## update iterator
          count <- count + 1
        }
      }
    } else {
      cat("Cannot compute PAPDp")
    }

    ## AUPEC
    aupec <- vector("list", length = length(algorithms))
    for (i in seq_along(algorithms)) {

      aupec[[i]] <- AUPEC(Tcv, fit_ml[[i]][["tau"]], Ycv, centered = TRUE)
      
    }

    ## GATE
    GATE <- vector("list", length = length(algorithms))
    for (i in seq_along(algorithms)) {

      ## Compute GATE
      GATE[[i]] <- GATE(Tcv, fit_ml[[i]][["tau"]], Ycv, params$ngates)

      ## indicate algorithm
      GATE[[i]]$alg <- algorithms[i]

      ## indicate group number
      GATE[[i]]$group <- seq_along(GATE[[i]]$gate)
    }

    ## URATE
    URATE <- vector("list", length = length(algorithms))
    for (i in seq_along(algorithms)) {

      ## Compute URATE
      URATE[[i]] <- URATE(Tcv, fit_ml[[i]][["tau"]], Ycv)

      ## indicate algorithm
      URATE[[i]]$alg <- algorithms[i]

    }

  }

  out <- list(
        PAPE = PAPE,
        PAPEp = PAPEp,
        PAPDp = PAPDp,
        AUPEC = aupec,
        GATE = GATE,
        URATE = URATE)

  return(out)
}



#' Compute Quantities of Interest (PAPE, PAPEp, PAPDp, AUPEC, GATE, GATEcv, URATE) with user defined functions
#' @param user_itr A user-defined function to create an ITR. The function should take the data as input and return an unit-level continuous score for treatment assignment. We assume those that have score less than 0 should not have treatment. The default is \code{NULL}, which means the ITR will be estimated from the \code{estimate_itr}. 
#' @param Tcv A vector of the unit-level binary treatment.
#' @param Ycv A vector of the unit-level continuous outcome.
#' @param data A data frame containing the variables of interest.
#' @param ngates The number of gates to be used in the GATE function.
#' @param budget The maximum percentage of population that can be treated under the budget constraint.
#' @param ... Additional arguments to be passed to the user-defined function.
#' @importFrom rlang .data
compute_qoi_user <- function(user_itr, Tcv, Ycv, data, ngates, budget, ...) {

  # parameters
  function_name <- as.character(substitute(user_itr))

  # ITR
  tau <- do.call(user_itr, list(data))
  That <- ifelse(tau >= 0, 1, 0)

  # ITR with budget constraint
  That_p <- numeric(length(That))
  That_p[sort(tau,decreasing =TRUE,index.return=TRUE)$ix[1:(floor(budget*length(tau))+1)]] = 1

  # PAPE 
  PAPE <- PAPEp <- vector("list", length(user_itr))
  for (i in seq_len(length(user_itr))) {

    ## compute PAPE
    PAPE[[i]] <- PAPE(Tcv, That, Ycv, centered = TRUE)

    ## compute PAPEp
    PAPEp[[i]] <- PAPE(Tcv, That_p, Ycv, centered = TRUE, budget)

    ## name
    PAPE[[i]]$alg <- function_name[i]
    
    PAPEp[[i]]$alg <- function_name[i]

  }

  ## AUPEC
  aupec <- vector("list", length = length(user_itr))
  for (i in seq_along(length(user_itr))) {
    
    # compute AUPEC
    aupec[[i]] <- AUPEC(Tcv, tau, Ycv, centered = TRUE)

    # name
    aupec[[i]]$alg <- function_name[i]
  }

  ## GATE
  GATE <- vector("list", length = length(user_itr))
  for (i in seq_along(length(user_itr))) {

    ## Compute GATE
    GATE[[i]] <- GATE(Tcv, tau, Ycv, ngates)

    ## indicate algorithm
    GATE[[i]]$alg <- function_name[i]

    ## indicate group number
    GATE[[i]]$group <- seq_along(GATE[[i]]$gate)
  }

  ## URATE
  URATE <- vector("list", length = length(user_itr))
  for (i in seq_along(length(user_itr))) {

    ## Compute URATE
    URATE[[i]] <- URATE(Tcv, tau, Ycv)

    ## indicate algorithm
    URATE[[i]]$alg <- function_name[i]

  }
  
  out <- list(
        PAPE = PAPE,
        PAPEp = PAPEp,
        PAPDp = NULL, # single algorithm
        AUPEC = aupec,
        GATE = GATE,
        URATE = URATE)

  return(out)
}
