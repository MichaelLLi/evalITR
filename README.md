
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

## Example

This is an example using the STAT dataset. We first load the `star`
dataset and specify outcome variables (reading, math, and writing) and
covariates we want to include in the model. Then we use machine learning
algorithms to estimate the heterogeneous effects of small classes on
educational attainment. We use 20% as a budget constraint and tuned the
model through through the 3-fold cross validation.

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

We plot the estimated Area Under the Prescriptive Effect Curve (AUPEC)
for the writing score across a range of budget constraints for different
machine learning algorithms.

``` r
# plot the AUPEC with different ML algorithms
plot_aupec(fit = fit$qoi[1], 
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

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->
