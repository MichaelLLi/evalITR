
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

## Example (cross-validation)

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

# train the model
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

``` r
# compute estimates
est <- estimate_itr(
          fit = fit,
          outcome = "g3tlangss",
          algorithms = c(
                "causal_forest", 
                "bart",
                "lasso",
                "boost", 
                "random_forest",
                "bagging",
                "cart"))
```

``` r
# summarize estimates
summary(est)
#> ── PAPE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     1.01          1.02 causal_forest      0.99   0.321
#> 2     0.61          1.12          bart      0.55   0.583
#> 3     0.62          1.26         lasso      0.49   0.624
#> 4     1.61          0.84         boost      1.92   0.055
#> 5     2.69          1.31 random_forest      2.04   0.041
#> 6     1.36          1.26       bagging      1.08   0.282
#> 7     0.84          0.89          cart      0.94   0.345
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1    2.728          0.93 causal_forest     2.938  0.0033
#> 2    0.916          1.11          bart     0.828  0.4074
#> 3   -0.014          0.65         lasso    -0.022  0.9826
#> 4    1.507          0.66         boost     2.277  0.0228
#> 5    2.343          1.18 random_forest     1.981  0.0475
#> 6    1.234          0.70       bagging     1.765  0.0775
#> 7    1.680          0.62          cart     2.709  0.0068
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#>    estimate std.deviation                     algorithm statistic p.value
#> 1      1.81          0.69          causal_forest x bart      2.63  0.0084
#> 2      2.74          0.85         causal_forest x lasso      3.21  0.0013
#> 3      1.22          0.80         causal_forest x boost      1.52  0.1281
#> 4      0.39          0.92 causal_forest x random_forest      0.42  0.6749
#> 5      1.49          0.77       causal_forest x bagging      1.94  0.0518
#> 6      1.05          0.91          causal_forest x cart      1.16  0.2479
#> 7      0.93          0.78                  bart x lasso      1.19  0.2351
#> 8     -0.59          0.80                  bart x boost     -0.74  0.4595
#> 9     -1.43          1.24          bart x random_forest     -1.15  0.2501
#> 10    -0.32          1.22                bart x bagging     -0.26  0.7949
#> 11    -0.76          0.89                   bart x cart     -0.86  0.3921
#> 12    -1.52          1.29                 lasso x boost     -1.18  0.2398
#> 13    -2.36          0.88         lasso x random_forest     -2.68  0.0073
#> 14    -1.25          0.85               lasso x bagging     -1.47  0.1418
#> 15    -1.69          1.55                  lasso x cart     -1.09  0.2741
#> 16    -0.84          0.79         boost x random_forest     -1.06  0.2913
#> 17     0.27          0.74               boost x bagging      0.37  0.7134
#> 18    -0.17          1.48                  boost x cart     -0.12  0.9070
#> 19     1.11          0.58       random_forest x bagging      1.92  0.0547
#> 20     0.66          0.94          random_forest x cart      0.71  0.4791
#> 21    -0.45          0.91                bagging x cart     -0.49  0.6229
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     1.61          1.73 causal_forest      0.93    0.35
#> 2     1.21          1.51          bart      0.81    0.42
#> 3     0.37          1.28         lasso      0.29    0.77
#> 4     1.27          1.08         boost      1.17    0.24
#> 5     1.86          1.26 random_forest      1.47    0.14
#> 6     0.78          0.86       bagging      0.91    0.36
#> 7     0.54          1.61          cart      0.34    0.74
```

The`summary()` function displays the following summary statistics: (1)
population average prescriptive effect `PAPE`; (2) population average
prescriptive effect with a budget constraint `PAPEp`; (3) population
average prescriptive effect difference with a budget constraint `PAPDp`;
(4) and area under the prescriptive effect curve `AUPEC`. For more
information about these evaluation metrics, please refer to [this
paper](https://arxiv.org/abs/1905.05389).

We plot the estimated Area Under the Prescriptive Effect Curve for the
writing score across a range of budget constraints for different
algorithms.

``` r
# plot the AUPEC with different ML algorithms
plot(x = est, 
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

## Example under sample splitting (under development)

Please set argument input of `n_folds` to 0 in order to train the models under sample splitting. The split ratio between train and test set is determined by the `ratio` argument. 

```r
library(tidyverse)
library(evalITR)

load("data/star.rda")

# specifying outcomes
outcomes <- c("g3tlangss",
              "g3treadss",
              "g3tmathss")


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
                  "lasso",
                  "boost", 
                  "random_forest",
                  "bagging",
                  "cart"),
               plim = 0.2,
               n_folds = 0, # train under sample splitting
               ratio = 0.67) # set 0.67 as the split ratio between train and test set

glimpse(fit$qoi)

List of 6
#> $ PAPE  :List of 6
#>  ..$ :List of 3
#>  .. ..$ pape: num 5.12
#>  .. ..$ sd  : num 0.642
#>  .. ..$ alg : chr "causal_forest"
#>  ..$ :List of 3
#>  .. ..$ pape: num 2.72
#>  .. ..$ sd  : num 0.808
#>  .. ..$ alg : chr "lasso"
#>  ..$ :List of 3
#>  .. ..$ pape: num 8.28
#>  .. ..$ sd  : num 0.871
#>  .. ..$ alg : chr "boost"
#>  ..$ :List of 3
#>  .. ..$ pape: num 13.6
#>  .. ..$ sd  : num 0.835
#>  .. ..$ alg : chr "random_forest"
#>  ..$ :List of 3
#>  .. ..$ pape: num 13.8
#>  .. ..$ sd  : num 0.832
#>  .. ..$ alg : chr "bagging"
#>  ..$ :List of 3
#>  .. ..$ pape: num 0.421
#>  .. ..$ sd  : num 0.901
#>  .. ..$ alg : chr "cart"
#> $ PAPEp :List of 6
#>  ..$ :List of 3
#>  .. ..$ pape: num 5.04
#>  .. ..$ sd  : num 0.711
#>  .. ..$ alg : chr "causal_forest"
#>  ..$ :List of 3
#>  .. ..$ pape: num 2.79
#>  .. ..$ sd  : num 0.72
#>  .. ..$ alg : chr "lasso"
#>  ..$ :List of 3
#>  .. ..$ pape: num 5.51
#>  .. ..$ sd  : num 0.712
#>  .. ..$ alg : chr "boost"
#>  ..$ :List of 3
#>  .. ..$ pape: num 9.6
#>  .. ..$ sd  : num 0.678
#>  .. ..$ alg : chr "random_forest"
#>  ..$ :List of 3
#>  .. ..$ pape: num 9.53
#>  .. ..$ sd  : num 0.675
#>  .. ..$ alg : chr "bagging"
#>  ..$ :List of 3
#>  .. ..$ pape: num -0.148
#>  .. ..$ sd  : num 0.709
#>  .. ..$ alg : chr "cart"
#> $ PAPDp :List of 15
#>  ..$ :List of 3
#>  .. ..$ papd: num 2.25
#>  .. ..$ sd  : num 0.853
#>  .. ..$ alg : chr "causal_forest x lasso"
#>  ..$ :List of 3
#>  .. ..$ papd: num -0.471
#>  .. ..$ sd  : num 0.814
#>  .. ..$ alg : chr "causal_forest x boost"
#>  ..$ :List of 3
#>  .. ..$ papd: num -4.56
#>  .. ..$ sd  : num 0.706
#>  .. ..$ alg : chr "causal_forest x random_forest"
#>  ..$ :List of 3
#>  .. ..$ papd: num -4.49
#>  .. ..$ sd  : num 0.813
#>  .. ..$ alg : chr "causal_forest x bagging"
#>  ..$ :List of 3
#>  .. ..$ papd: num 5.19
#>  .. ..$ sd  : num 1.03
#>  .. ..$ alg : chr "causal_forest x cart"
#>  ..$ :List of 3
#>  .. ..$ papd: num -2.72
#>  .. ..$ sd  : num 0.866
#>  .. ..$ alg : chr "lasso x boost"
#>  ..$ :List of 3
#>  .. ..$ papd: num -6.81
#>  .. ..$ sd  : num 0.9
#>  .. ..$ alg : chr "lasso x random_forest"
#>  ..$ :List of 3
#>  .. ..$ papd: num -6.74
#>  .. ..$ sd  : num 0.898
#>  .. ..$ alg : chr "lasso x bagging"
#>  ..$ :List of 3
#>  .. ..$ papd: num 2.94
#>  .. ..$ sd  : num 1.04
#>  .. ..$ alg : chr "lasso x cart"
#>  ..$ :List of 3
#>  .. ..$ papd: num -4.09
#>  .. ..$ sd  : num 0.787
#>  .. ..$ alg : chr "boost x random_forest"
#>  ..$ :List of 3
#>  .. ..$ papd: num -4.02
#>  .. ..$ sd  : num 0.824
#>  .. ..$ alg : chr "boost x bagging"
#>  ..$ :List of 3
#>  .. ..$ papd: num 5.66
#>  .. ..$ sd  : num 1.02
#>  .. ..$ alg : chr "boost x cart"
#>  ..$ :List of 3
#>  .. ..$ papd: num 0.0753
#>  .. ..$ sd  : num 0.487
#>  .. ..$ alg : chr "random_forest x bagging"
#>  ..$ :List of 3
#>  .. ..$ papd: num 9.75
#>  .. ..$ sd  : num 0.954
#>  .. ..$ alg : chr "random_forest x cart"
#>  ..$ :List of 3
#>  .. ..$ papd: num 9.68
#>  .. ..$ sd  : num 0.977
#>  .. ..$ alg : chr "bagging x cart"
#> $ AUPEC :List of 6
#>  ..$ :List of 3
#>  .. ..$ aupec: num 5.65
#>  .. ..$ sd   : num 0.583
#>  .. ..$ vec  : num [1:1911] -2.69 -2.69 -2.63 -2.68 -2.68 ...
#>  ..$ :List of 3
#>  .. ..$ aupec: num 2.58
#>  .. ..$ sd   : num 0.618
#>  .. ..$ vec  : num [1:1911] -2.63 -2.69 -2.69 -2.71 -2.75 ...
#>  ..$ :List of 3
#>  .. ..$ aupec: num 6.38
#>  .. ..$ sd   : num 0.705
#>  .. ..$ vec  : num [1:1911] -2.63 -2.56 -2.6 -2.61 -2.59 ...
#>  ..$ :List of 3
#>  .. ..$ aupec: num 11.1
#>  .. ..$ sd   : num 0.716
#>  .. ..$ vec  : num [1:1911] -2.63 -2.56 -2.47 -2.42 -2.39 ...
#>  ..$ :List of 3
#>  .. ..$ aupec: num 11.4
#>  .. ..$ sd   : num 0.716
#>  .. ..$ vec  : num [1:1911] -2.63 -2.56 -2.52 -2.42 -2.39 ...
#>  ..$ :List of 3
#>  .. ..$ aupec: num -0.475
#>  .. ..$ sd   : num 0.742
#>  .. ..$ vec  : num [1:1911] -2.75 -2.74 -2.73 -2.67 -2.67 ...
#> $ GATE  :List of 6
#>  ..$ :List of 4
#>  .. ..$ gate : num [1:5] -24.2 -81.8 79.6 -68.8 124
#>  .. ..$ sd   : num [1:5] 57.6 57.4 57.8 57.3 58
#>  .. ..$ alg  : chr "causal_forest"
#>  .. ..$ group: int [1:5] 1 2 3 4 5
#>  ..$ :List of 4
#>  .. ..$ gate : num [1:5] 12.03 -6.65 6.99 94.71 -78.2
#>  .. ..$ sd   : num [1:5] 57.5 57.8 57.9 57.6 57.5
#>  .. ..$ alg  : chr "lasso"
#>  .. ..$ group: int [1:5] 1 2 3 4 5
#>  ..$ :List of 4
#>  .. ..$ gate : num [1:5] -35.8 89.7 -48.7 55.6 -31.9
#>  .. ..$ sd   : num [1:5] 57.5 57.5 57.5 57.8 57.9
#>  .. ..$ alg  : chr "boost"
#>  .. ..$ group: int [1:5] 1 2 3 4 5
#>  ..$ :List of 4
#>  .. ..$ gate : num [1:5] -21.7 -76.4 42.8 16.7 67.5
#>  .. ..$ sd   : num [1:5] 57.6 57.2 57.4 57.4 58.5
#>  .. ..$ alg  : chr "random_forest"
#>  .. ..$ group: int [1:5] 1 2 3 4 5
#>  ..$ :List of 4
#>  .. ..$ gate : num [1:5] -58.1 -33.5 45.1 -24.5 99.9
#>  .. ..$ sd   : num [1:5] 57.5 57.4 57.4 57.2 58.6
#>  .. ..$ alg  : chr "bagging"
#>  .. ..$ group: int [1:5] 1 2 3 4 5
#>  ..$ :List of 4
#>  .. ..$ gate : num [1:5] 42.9 48.91 41.89 -103.68 -1.14
#>  .. ..$ sd   : num [1:5] 57.9 57.7 57.8 57.3 57.5
#>  .. ..$ alg  : chr "cart"
#>  .. ..$ group: int [1:5] 1 2 3 4 5
```
