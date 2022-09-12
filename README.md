
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
summary(fit,1, type = "PAPE")
#> ── Estimates ───────────────────────────────────────────────────────────────────
#>     pape   sd           alg
#> 1  1.062 0.92 causal_forest
#> 2 -0.146 0.96          bart
#> 3  0.042 0.87         lasso
#> 4  2.523 1.19         boost
#> 5  1.283 1.31 random_forest
#> 6  1.990 0.89       bagging
#> 7 -0.716 1.15          cart

# get PAPE estimates
summary(fit, 1, type = "PAPEp")
#> ── Estimates ───────────────────────────────────────────────────────────────────
#>   papep   sd           alg
#> 1  0.85 0.76 causal_forest
#> 2  0.54 0.78          bart
#> 3  0.63 0.66         lasso
#> 4  2.39 0.75         boost
#> 5  1.32 0.87 random_forest
#> 6  1.60 0.84       bagging
#> 7 -1.46 0.99          cart

# get PAPDp estimates
summary(fit, 1, type = "PAPDp")
#> ── Estimates ───────────────────────────────────────────────────────────────────
#>      papd   sd                           alg
#> 1   0.307 0.82          causal_forest x bart
#> 2   0.217 1.01         causal_forest x lasso
#> 3  -1.542 1.52         causal_forest x boost
#> 4  -0.468 1.40 causal_forest x random_forest
#> 5  -0.753 1.56       causal_forest x bagging
#> 6   2.309 1.01          causal_forest x cart
#> 7  -0.089 0.94                  bart x lasso
#> 8  -1.848 0.85                  bart x boost
#> 9  -0.774 0.90          bart x random_forest
#> 10 -1.060 0.88                bart x bagging
#> 11  2.003 1.04                   bart x cart
#> 12 -1.759 1.08                 lasso x boost
#> 13 -0.685 1.11         lasso x random_forest
#> 14 -0.970 1.33               lasso x bagging
#> 15  2.092 0.97                  lasso x cart
#> 16  1.074 1.41         boost x random_forest
#> 17  0.789 1.47               boost x bagging
#> 18  3.851 1.00                  boost x cart
#> 19 -0.285 1.27       random_forest x bagging
#> 20  2.777 1.13          random_forest x cart
#> 21  3.062 1.12                bagging x cart

# get AUPEC estimates
summary(fit, 1, type = "AUPEC")
#> ── Estimates ───────────────────────────────────────────────────────────────────
#>   aupec  sd     algorithm
#> 1 -0.35 1.4 causal_forest
#> 2 -0.96 1.4          bart
#> 3 -0.50 1.6         lasso
#> 4  1.44 1.3         boost
#> 5  0.40 1.4 random_forest
#> 6  0.83 1.6       bagging
#> 7 -1.36 1.1          cart
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
