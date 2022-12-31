#' Compute Quantities of Interest (PAPE, PAPEp, PAPDp, AUPEC, GATE, GATEcv)
#' @param fit_obj An output object from \code{itr_single_outcome} function.
#' @param algorithms Machine learning algorithms
#' @param cv Indicate training method. Set \code{cv = TRUE} for cross validation and \code{FALSE} for sample splitting.
#' @importFrom rlang .data
# compute_qoi <- function(fit_obj, algorithms) {

compute_qoi <- function(fit_obj,
                        algorithms,
                        cv,
                        quantities) {

  ## extract objects
  fit_ml <- fit_obj$fit_ml
  params <- fit_obj$params
  Ycv    <- fit_obj$Ycv
  Tcv    <- fit_obj$Tcv
  indcv  <- fit_obj$indcv
  plim   <- fit_obj$plim
  cv     <- cv
  quantities  <- quantities


  if (cv == TRUE) {

    ## compute quantities under cross validation
    ## PAPE and PAPEp
    if ("PAPE" %in% quantities) {
      PAPE <- PAPEp <- vector("list", params$n_alg)
      for (i in seq_len(params$n_alg)) {
        ## make That_cv into matrix
        That_cv_mat <- furrr::future_map(fit_ml[[ algorithms[i] ]], ~.x$That_cv) %>% do.call(cbind, .)
        That_pcv_mat <- furrr::future_map(fit_ml[[ algorithms[i] ]], ~.x$That_pcv) %>% do.call(cbind, .)

        ## compute PAPE
        PAPE[[i]] <- PAPEcv(Tcv, That_cv_mat, Ycv, indcv)

        ## compute PAPEp
        PAPEp[[i]] <- PAPEcv(Tcv, That_pcv_mat, Ycv, indcv, plim)

        ## name
        PAPE[[i]]$alg <-  PAPEp[[i]]$alg <- algorithms[i]
      }
    }

    ## PAPDp
    if ("PAPDp" %in% quantities) {
      PAPDp <- list()
      if (params$n_alg > 1) {
        count <- 1

        for (i in 1:(params$n_alg-1)) {
          That_pcv_i <- furrr::future_map(fit_ml[[ algorithms[i] ]], ~.x$That_pcv) %>% do.call(cbind, .)

          for (j in (i+1):params$n_alg) {
            # compare algorithm[i] and algorithm[j]
            That_pcv_j <- furrr::future_map(fit_ml[[ algorithms[j] ]], ~.x$That_pcv) %>% do.call(cbind, .)

            PAPDp[[count]] <- PAPDcv(
              Tcv, That_pcv_i, That_pcv_j, Ycv, indcv, plim
            )

            PAPDp[[count]]$alg <- paste0(algorithms[i], " x ", algorithms[j])
            ## update iterator
            count <- count + 1
          }
        }
      } else {
        cat("Cannot compute PAPDp")
      }
    }


    ##  Compute AUPEC
    if ("AUPEC" %in% quantities) {
      aupec <- vector("list", length = length(algorithms))
      for (i in seq_along(algorithms)) {
        tau <- furrr::future_map(fit_ml[[i]], ~.x$tau) %>% do.call(cbind, .)
        tau_cv <- furrr::future_map(fit_ml[[i]], ~.x$tau_cv) %>% do.call(cbind, .)
        That_pcv_mat <- furrr::future_map(fit_ml[[i]], ~.x$That_pcv) %>% do.call(cbind, .)

        aupec[[i]] <- getAupecOutput_cv(
          tau, tau_cv, That_pcv_mat, algorithms[i],
          NFOLDS = params$n_folds, Ycv = Ycv, Tcv = Tcv, indcv = indcv
        )
      }
    }

    ## GATEcv
    if ("GATE" %in% quantities) {
      GATEcv <- vector("list", length = length(algorithms))
      for (i in seq_along(algorithms)) {
        tau <- furrr::future_map(fit_ml[[i]], ~.x$tau) %>% do.call(cbind, .)
        tau_cv <- furrr::future_map(fit_ml[[i]], ~.x$tau_cv) %>% do.call(cbind, .)

        ## Compute GATE
        GATEcv[[i]] <- GATEcv(Tcv, tau_cv, Ycv, indcv, params$ngates) #should enable user to set ngates

        ## indicate algorithm
        GATEcv[[i]]$alg <- algorithms[i]

        ## indicate group number
        GATEcv[[i]]$group <- seq_along(GATEcv[[i]]$gate)
      }
    }


  } else {
    # compute quantities under sample splitting

    ## PAPE and PAPEp
    PAPE <- PAPEp <- vector("list", params$n_alg)
    for (i in seq_len(params$n_alg)) {

      ## compute PAPE
      PAPE[[i]] <- PAPE(Tcv, fit_ml[[i]][[3]], Ycv, centered = TRUE)

      ## compute PAPEp: sp does not have papep, check PAPE.R
      PAPEp[[i]] <- PAPE(Tcv, fit_ml[[i]][[4]], Ycv, centered = TRUE, plim)

      ## name
      PAPE[[i]]$alg <-  PAPEp[[i]]$alg <- algorithms[i]
    }


    ## PAPDp
    PAPDp <- list()
    if (params$n_alg > 1) {
      count <- 1

      for (i in 1:(params$n_alg-1)) {
        That_pcv_i <- fit_ml[[i]][[4]]

        for (j in (i+1):params$n_alg) {
          # compare algorithm[i] and algorithm[j]
          That_pcv_j <- fit_ml[[j]][[4]]

          PAPDp[[count]] <- PAPD(
            Tcv, That_pcv_i, That_pcv_j, Ycv, plim, centered = TRUE
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

      aupec[[i]] <- AUPEC(Tcv, fit_ml[[i]][[1]], Ycv, centered = TRUE)
    }

    ## GATE
    GATE <- vector("list", length = length(algorithms))
    for (i in seq_along(algorithms)) {

      ## Compute GATE
      GATE[[i]] <- GATE(Tcv, fit_ml[[i]][[1]], Ycv, params$ngates)

      ## indicate algorithm
      GATE[[i]]$alg <- algorithms[i]

      ## indicate group number
      GATE[[i]]$group <- seq_along(GATE[[i]]$gate)
    }
  }

  return(

    if (cv == TRUE) {
      list(
        # PAPEp = PAPEp,
        PAPE = ifelse("PAPE" %in% quantities, list(PAPE), as.factor(NA)), #only returns result of the first algorithm of a quantity
        PAPEp = ifelse("PAPE" %in% quantities, list(PAPEp), as.factor(NA)),
        PAPDp = ifelse("PAPDp" %in% quantities, list(PAPDp), as.factor(NA)),
        AUPEC = ifelse("AUPEC" %in% quantities, list(aupec), as.factor(NA)),
        GATEcv = ifelse("GATE" %in% quantities, list(GATEcv), as.factor(NA))
      )

    } else {

      list(
        # PAPE = PAPE,
        # PAPEp = PAPEp,
        # PAPDp = PAPDp,
        # AUPEC = aupec,
        # GATE = GATE

        PAPE = ifelse("PAPE" %in% quantities, list(PAPE), as.factor(NA)), #only returns result of the first algorithm of a quantity
        PAPEp = ifelse("PAPE" %in% quantities, list(PAPEp), as.factor(NA)),
        PAPDp = ifelse("PAPDp" %in% quantities, list(PAPDp), as.factor(NA)),
        AUPEC = ifelse("AUPEC" %in% quantities, list(aupec), as.factor(NA)),
        GATE = ifelse("GATE" %in% quantities, list(GATE), as.factor(NA))
      )

    }

  )
}
