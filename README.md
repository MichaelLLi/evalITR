
<!-- README.md is generated from README.Rmd. Please edit that file -->

# evalITR

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of evalITR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MichaelLLi/evalITR")
```

(Optional) if you have multiple cores, we recommendate using
multisession futures and processing in parallel. This would increase
computation efficiency and reduce the time to fit the model.

``` r
library(furrr)
library(future.apply)

nworkers <- 4
plan(multisession, workers =nworkers)
```

## Example

This is an example using the `star` dataset (for more information about
the dataset, please use `?star`).

We first load the dataset and specify both outcome variables (reading,
math, and writing scores) and covariates we want to include in the
model. Then we use a series of machine learning algorithms to estimate
the heterogeneous effects of small classes on educational attainment. We
use 20% as a budget constraint and tuned the model through through the
3-fold cross validation.

``` r
library(tidyverse)
library(evalITR)

load("data/star.rda")

# specifying outcomes
outcomes <- c("g3tlangss",
                "g3treadss","g3tmathss")


# specifying covariates
covariates <-  star %>% 
                dplyr::select(-c(all_of(outcomes),"treatment")) %>% 
                colnames()

# estimate ITR 
fit <- run_itr(outcome = outcomes,
               treatment = "treatment",
               covariates = covariates,
               data = star,
               algorithms = c(
                  "causal_forest", 
                  "bart",
                  "lasso",
                  "boost", 
                  "random_forest",
                  "bagging",
                  "cart"),
               plim = 0.2,
               n_folds = 3)
```

The`summary()` function display the following summary statistics: (1)
population average prescriptive effect `PAPE`; (2) population average
prescriptive effect with a budget constraint `PAPEp`; (3) population
average prescriptive effect difference with a budget constraint `PAPDp`;
(4) and area under the prescriptive effect curve `AUPEC`. For more
information about these evaluation metrics, please refer to [this
paper](https://arxiv.org/abs/1905.05389).

``` r
# get PAPE estimates
summary(fit, 1, type = "PAPE")
#> ── Estimates ───────────────────────────────────────────────────────────────────
#>    pape   sd           alg
#> 1 -0.40 0.56 causal_forest
#> 2 -0.11 1.24          bart
#> 3  0.81 1.25         lasso
#> 4  1.56 1.29         boost
#> 5  2.76 1.01 random_forest
#> 6  2.75 1.12       bagging
#> 7  1.84 1.16          cart

# get PAPEp estimates
summary(fit, 1, type = "PAPEp")
#> ── Estimates ───────────────────────────────────────────────────────────────────
#>   papep   sd           alg
#> 1  0.48 0.95 causal_forest
#> 2  1.28 1.05          bart
#> 3  0.73 0.88         lasso
#> 4  1.15 0.65         boost
#> 5  0.82 0.71 random_forest
#> 6  1.29 1.06       bagging
#> 7  1.26 1.10          cart

# get PAPDp estimates
summary(fit, 1, type = "PAPDp")
#> ── Estimates ───────────────────────────────────────────────────────────────────
#>       papd   sd                           alg
#> 1  -0.7973 0.60          causal_forest x bart
#> 2  -0.2439 1.27         causal_forest x lasso
#> 3  -0.6641 0.97         causal_forest x boost
#> 4  -0.3328 0.77 causal_forest x random_forest
#> 5  -0.8030 1.02       causal_forest x bagging
#> 6  -0.7758 1.51          causal_forest x cart
#> 7   0.5534 0.99                  bart x lasso
#> 8   0.1332 0.84                  bart x boost
#> 9   0.4644 0.97          bart x random_forest
#> 10 -0.0057 1.36                bart x bagging
#> 11  0.0215 1.49                   bart x cart
#> 12 -0.4202 0.71                 lasso x boost
#> 13 -0.0889 0.77         lasso x random_forest
#> 14 -0.5590 0.79               lasso x bagging
#> 15 -0.5318 1.47                  lasso x cart
#> 16  0.3313 0.75         boost x random_forest
#> 17 -0.1389 0.77               boost x bagging
#> 18 -0.1117 0.91                  boost x cart
#> 19 -0.4701 0.90       random_forest x bagging
#> 20 -0.4429 0.88          random_forest x cart
#> 21  0.0272 1.25                bagging x cart

# get AUPEC estimates
summary(fit, 1, type = "AUPEC")
#> ── Estimates ───────────────────────────────────────────────────────────────────
#>   aupec   sd     algorithm
#> 1  0.20 1.06 causal_forest
#> 2  0.98 0.90          bart
#> 3  0.26 1.44         lasso
#> 4  1.04 1.03         boost
#> 5  1.69 0.66 random_forest
#> 6  1.57 0.73       bagging
#> 7  1.19 1.04          cart
```

We plot the estimated Area Under the Prescriptive Effect Curve for the
writing score across a range of budget constraints for different
algorithms.

``` r
# plot the AUPEC with different ML algorithms
plot(x = fit, 
      outcome = outcomes[1],
      treatment = "treatment",
      data = star, 
      algorithms = c(
          "causal_forest",
          "bart",
          "lasso",
          "boost", 
          "random_forest",
          "bagging", 
          "cart"))
```

![](man/figures/README-plot-1.png)<!-- -->
