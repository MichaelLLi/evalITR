
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

The`summary()` function displays the following summary statistics: (1)
population average prescriptive effect `PAPE`; (2) population average
prescriptive effect with a budget constraint `PAPEp`; (3) population
average prescriptive effect difference with a budget constraint `PAPDp`;
(4) and area under the prescriptive effect curve `AUPEC`. For more
information about these evaluation metrics, please refer to [this
paper](https://arxiv.org/abs/1905.05389).

``` r
# summarize estimates
summary(fit)
#> ── PAPE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     0.65          1.69 causal_forest      0.38   0.702
#> 2     1.80          1.09          bart      1.65   0.099
#> 3     2.02          1.08         lasso      1.88   0.061
#> 4     0.61          0.83         boost      0.74   0.462
#> 5     1.97          1.28 random_forest      1.55   0.122
#> 6     1.18          0.84       bagging      1.40   0.161
#> 7     1.59          1.82          cart      0.88   0.381
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     1.26          1.08 causal_forest      1.16   0.245
#> 2     1.14          0.65          bart      1.77   0.077
#> 3     0.74          1.07         lasso      0.69   0.487
#> 4     1.67          0.66         boost      2.54   0.011
#> 5     0.79          0.68 random_forest      1.17   0.243
#> 6     0.46          0.69       bagging      0.67   0.501
#> 7     0.87          0.66          cart      1.31   0.191
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#>    estimate std.deviation                     algorithm statistic p.value
#> 1     0.116          0.55          causal_forest x bart     0.211    0.83
#> 2     0.515          1.30         causal_forest x lasso     0.396    0.69
#> 3    -0.410          0.96         causal_forest x boost    -0.427    0.67
#> 4     0.465          0.79 causal_forest x random_forest     0.593    0.55
#> 5     0.794          0.85       causal_forest x bagging     0.930    0.35
#> 6     0.391          1.19          causal_forest x cart     0.329    0.74
#> 7     0.399          0.76                  bart x lasso     0.526    0.60
#> 8    -0.526          1.46                  bart x boost    -0.361    0.72
#> 9     0.350          1.13          bart x random_forest     0.310    0.76
#> 10    0.678          1.42                bart x bagging     0.478    0.63
#> 11    0.276          1.39                   bart x cart     0.199    0.84
#> 12   -0.925          0.76                 lasso x boost    -1.209    0.23
#> 13   -0.049          0.86         lasso x random_forest    -0.057    0.95
#> 14    0.279          0.85               lasso x bagging     0.327    0.74
#> 15   -0.123          1.16                  lasso x cart    -0.107    0.91
#> 16    0.875          0.77         boost x random_forest     1.140    0.25
#> 17    1.204          1.29               boost x bagging     0.935    0.35
#> 18    0.801          1.47                  boost x cart     0.544    0.59
#> 19    0.328          0.46       random_forest x bagging     0.708    0.48
#> 20   -0.074          0.86          random_forest x cart    -0.086    0.93
#> 21   -0.402          1.40                bagging x cart    -0.288    0.77
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     1.06           2.3 causal_forest      0.46    0.65
#> 2     0.93           1.8          bart      0.51    0.61
#> 3     0.89           2.4         lasso      0.37    0.71
#> 4     0.39           1.6         boost      0.25    0.81
#> 5     1.04           2.2 random_forest      0.47    0.64
#> 6     0.47           2.5       bagging      0.19    0.85
#> 7     0.91           1.8          cart      0.51    0.61
```

We plot the estimated Area Under the Prescriptive Effect Curve for the
writing score across a range of budget constraints for different
algorithms.

``` r
# plot the AUPEC with different ML algorithms
plot(x = fit, 
      outcome = "g3tlangss",
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

<img src="man/figures/README-plot-1.png" style="display: block; margin: auto;" />
