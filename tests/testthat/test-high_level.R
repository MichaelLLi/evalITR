library(evalITR)
library(dplyr)
test_that("Sample Splitting Works", {
  load("star.rda")
  # specifying the outcome
  outcomes <- "g3tlangss"

  # specifying the treatment
  treatment <- "treatment"

  # specifying the data (remove other outcomes)
  star_data <- star %>% dplyr::select(-c(g3treadss,g3tmathss))

  # specifying the formula
  user_formula <- as.formula(
    "g3tlangss ~ treatment + gender + race + birthmonth +
  birthyear + SCHLURBN + GRDRANGE + GKENRMNT + GKFRLNCH +
  GKBUSED + GKWHITE ")


  # estimate ITR
  fit <- estimate_itr(
    treatment = treatment,
    form = user_formula,
    data = star_data,
    algorithms = c("lasso"),
    budget = 0.2,
    split_ratio = 0.7)
  expect_no_error(estimate_itr(
    treatment = treatment,
    form = user_formula,
    data = star_data,
    algorithms = c("lasso"),
    budget = 0.2,
    split_ratio = 0.7))


  # evaluate ITR
  est <- evaluate_itr(fit)
  expect_no_error(evaluate_itr(fit))
})

