
## user-defined functions

run_user <- function(
  dat_train,
  dat_test,
  dat_total,
  params,
  indcv,
  iter,
  budget,
  train_method,
  ...
) {

  # split/cross-validation
  cv <- params$cv

  ## train
  if(cv == TRUE){
    fit_user <- do.call(train_method, list(dat_train, dat_total))
  }

  if(cv == FALSE){
    fit_user <- do.call(train_method, list(dat_train, dat_test))
  }

  ## test
  fit_test <- test_user(
    fit_user, dat_test, dat_total, params$n_df, params$n_tb,
    indcv, iter, budget, cv
  )

  return(list(test = fit_test, train = fit_user$fit))
}



#'@importFrom stats predict runif
test_user <- function(
  fit_user, dat_test, dat_total, n_df, n_tb, indcv, iter, budget, cv
) {

  if(cv == TRUE){
    # predict
    tau_total= fit_user$score

    ## compute quantities of interest
    tau_test <-  tau_total[indcv == iter]
    That     <-  fit_user$itr
    That_p   <- as.numeric(tau_total >= sort(tau_test, decreasing = TRUE)[floor(budget*length(tau_test))+1])

    ## output
    cf_output <- list(
      tau      = c(tau_test, rep(NA, length(tau_total) - length(tau_test))),
      tau_cv   = tau_total,
      That_cv  = That,
      That_pcv = That_p
    )
  }

  if(cv == FALSE){
    ## predict
    tau_test= fit_user$score

    ## compute quantities of interest
    That     =  fit_user$itr
    That_p   = numeric(length(That))
    That_p[sort(tau_test,decreasing =TRUE,index.return=TRUE)$ix[1:(floor(budget*length(tau_test))+1)]] = 1

    ## output
    cf_output <- list(
      tau      = tau_test,
      tau_cv   = tau_test,
      That_cv  = That,
      That_pcv = That_p
      )
  }

  return(cf_output)
}

# #'@importFrom stats predict runif
# test_user <- function(
#   fit_user, dat_test, dat_total, n_df, n_tb, indcv, iter, budget, cv
# ) {

#   if(cv == TRUE){
#     # predict
#     tau_total= fit_user$score

#     ## compute quantities of interest
#     tau_test <-  tau_total[indcv == iter]
#     That     <-  fit_user$itr
#     That_p   <- as.numeric(tau_total >= sort(tau_test, decreasing = TRUE)[floor(budget*length(tau_test))+1])

#     ## output
#     cf_output <- list(
#       tau      = c(tau_test, rep(NA, length(tau_total) - length(tau_test))),
#       tau_cv   = tau_total,
#       That_cv  = That,
#       That_pcv = That_p
#     )
#   }

#   if(cv == FALSE){
#     ## predict
#     tau_test= fit_user$score

#     ## compute quantities of interest
#     That     =  fit_user$itr
#     That_p   = numeric(length(That))
#     That_p[sort(tau_test,decreasing =TRUE,index.return=TRUE)$ix[1:(floor(budget*length(tau_test))+1)]] = 1

#     ## output
#     cf_output <- list(
#       tau      = tau_test,
#       tau_cv   = tau_test,
#       That_cv  = That,
#       That_pcv = That_p
#       )
#   }

#   return(cf_output)
# }


