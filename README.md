
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/README-manual.png" width="100%" />

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

## Example under sample splitting

This is an example using the `star` dataset (for more information about
the dataset, please use `?star`).

We start with a simple example with one outcome variable (writing
scores) and one machine learning algorithm (causal forest). Then we move
to incoporate multiple outcomes and compare model performances with
several machine learning algorithms.

To begin, we load the dataset and specify the outcome variable and
covariates to be used in the model. Next, we utilize a random forest
algorithm to develop an Individualized Treatment Rule (ITR) for
estimating the varied impacts of small class sizes on students’ writing
scores. Since the treatment is often costly for most policy programs, we
consider a case with 20% budget constraint (`budget` = 0.2). The model
will identify the top 20% of units who benefit from the treatment most
and assign them to with the treatment. We train the model through sample
splitting, with the `ratio` between the train and test sets determined
by the ratio argument. Specifically, we allocate 70% of the data to
train the model, while the remaining 30% is used as testing data
(`ratio` = 0.7).

``` r
library(tidyverse)
library(evalITR)

load("data/star.rda")

# specifying the outcome
outcomes <- "g3tlangss"

# specifying the treatment
treatment <- "treatment"

# specifying covariates
covariates <-  star %>% dplyr::select(-c("g3tlangss",
                "g3treadss","g3tmathss","treatment")) %>% 
                colnames()

# specifying the data
star_data = star %>% dplyr::select(-c(g3treadss,g3tmathss))

# specifying the formula
user_formula <- as.formula(paste(paste0(outcomes)," ~ ", paste0(covariates, collapse = "+"), " + treatment"))


# estimate ITR 
fit <- estimate_itr(
               treatment = treatment,
               form = user_formula,
               data = star_data,
               algorithms = c("causal_forest"),
               budget = 0.2,
               ratio = 0.7)
#> Evaluate ITR under sample splitting ...


# evaluate ITR 
est <- evaluate_itr(fit)
#> Cannot compute PAPDp
```

Alternatively, we can train the model with the `caret` package (for
further information about `caret`, see
[caret](http://topepo.github.io/caret/index.html)).

``` r
# alternatively (with caret package)

# specify the trainControl method
fitControl <- caret::trainControl(## 3-fold CV
                           method = "repeatedcv",
                           number = 3,
                           ## repeated 3 times
                           repeats = 3)

# specify the tuning grid
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

# estimate ITR
fit_caret <- estimate_itr(
              treatment = "treatment",
              form = user_formula,
              trControl = fitControl,
              data = star_data,
              algorithms = c("gbm"),
              budget = 0.2,
              ratio = 0.7,
              tuneGrid = gbmGrid,
              verbose = FALSE)

# evaluate ITR
est_caret <- evaluate_itr(fit_caret)

# check the final model
est_caret$estimates$models$gbm$finalModel
```

The`summary()` function displays the following summary statistics: (1)
population average prescriptive effect `PAPE`; (2) population average
prescriptive effect with a budget constraint `PAPEp`; (3) population
average prescriptive effect difference with a budget constraint `PAPDp`.
This quantity will be computed with more than 2 machine learning
algorithms); (4) and area under the prescriptive effect curve `AUPEC`.
For more information about these evaluation metrics, please refer to
[Imai and Li (2021)](https://arxiv.org/abs/1905.05389); (5) Grouped
Average Treatment Effects `GATEs`. The details of the methods for this
design are given in [Imai and Li
(2022)](https://arxiv.org/abs/2203.14511).

``` r
# summarize estimates
summary(est)
#> ── PAPE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1    -0.49          0.95 causal_forest     -0.51    0.61
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      1.8           1.1 causal_forest       1.6     0.1
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#> data frame with 0 columns and 0 rows
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      1.2          0.84 causal_forest       1.5    0.15
#> 
#> ── GATE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm group statistic p.value upper lower
#> 1    120.3           109 causal_forest     1     1.108    0.27  -181   177
#> 2   -110.1           108 causal_forest     2    -1.023    0.31  -179   175
#> 3     -2.7           108 causal_forest     3    -0.025    0.98  -179   175
#> 4   -111.7           107 causal_forest     4    -1.047    0.29  -177   174
#> 5     94.7           108 causal_forest     5     0.876    0.38  -180   176

# summarize estimates with model trained using caret
# summary(est_caret)
```

We plot the estimated Area Under the Prescriptive Effect Curve for the
writing score across a range of budget constraints for causal forest.

``` r
# plot the AUPEC 
plot(est)
```

<img src="man/figures/README-sp_plot-1.png" style="display: block; margin: auto;" />

``` r

# plot the AUPEC for the model trained using caret
# plot(est_caret)
```

## Example under cross-validation

The package also allows estimate ITR with k-folds cross-validation.
Instead of specifying the `ratio` argument, we choose the number of
folds (`n_folds`). The following code presents an example of estimating
ITR with 3 folds cross-validation. In practice, we recommend using 10
folds to get a more stable model performance.

``` r
# estimate ITR 
set.seed(2021)
fit_cv <- estimate_itr(
               treatment = treatment,
               form = user_formula,
               data = star_data,
               trcontrol = fitControl,
               algorithms = c("causal_forest"),
               budget = 0.2,
               n_folds = 3)
#> Evaluate ITR with cross-validation ...

# evaluate ITR 
est_cv <- evaluate_itr(fit_cv)
#> Cannot compute PAPDp

# summarize estimates
summary(est_cv)
#> ── PAPE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     0.49          0.91 causal_forest      0.54    0.59
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      2.6          0.76 causal_forest       3.4   6e-04
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#> data frame with 0 columns and 0 rows
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      1.2           1.5 causal_forest      0.81    0.42
#> 
#> ── GATE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm group statistic p.value upper lower
#> 1      -85            59 causal_forest     1     -1.45    0.15   -93   101
#> 2       40            59 causal_forest     2      0.68    0.50   -94   101
#> 3       29            59 causal_forest     3      0.50    0.62   -93   101
#> 4       13            59 causal_forest     4      0.22    0.82   -94   101
#> 5       21           102 causal_forest     5      0.20    0.84  -164   171

# plot the AUPEC 
plot(est_cv)
```

![](man/figures/README-cv_estimate-1.png)<!-- -->

## Example with multiple ML algorithms

We can estimate ITR with various machine learning algorithms and then
compare the performance of each model. The package includes all ML
algorithms in the `caret` package and 2 additional algorithms ([causal
forest](https://grf-labs.github.io/grf/reference/causal_forest.html) and
[bartCause](https://cran.r-project.org/web/packages/bartCause/index.html)).

The package also allows estimate heterogeneous treatment effects on the
individual and group-level. On the individual-level, the summary
statistics and the AUPEC plot show whether assigning individualized
treatment rules may outperform complete random experiment. On the
group-level, we specify the number of groups through `ngates` and
estimating heterogeneous treatment effects across groups.

``` r
# specify the trainControl method
fitControl <- caret::trainControl(
                           method = "repeatedcv",
                           number = 3,
                           repeats = 3)
# estimate ITR
set.seed(2021)
fit_cv <- estimate_itr(
               treatment = "treatment",
               form = user_formula,
               data = star_data,
               trControl = fitControl,
               algorithms = c(
                  "causal_forest", 
                  "bartc",
                  "lasso", # from caret package
                  "rf"), 
               budget = 0.2,
               n_folds = 3)
#> Evaluate ITR with cross-validation ...
#> fitting treatment model via method 'bart'
#> fitting response model via method 'bart'
#> fitting treatment model via method 'bart'
#> fitting response model via method 'bart'
#> fitting treatment model via method 'bart'
#> fitting response model via method 'bart'

# evaluate ITR
est_cv <- evaluate_itr(fit_cv)

# summarize estimates
summary(est_cv)
#> ── PAPE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     0.36          0.93 causal_forest      0.39   0.697
#> 2    -0.20          0.40         bartc     -0.51   0.613
#> 3    -0.29          0.22         lasso     -1.29   0.197
#> 4     1.35          0.74            rf      1.83   0.067
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     2.38          0.66 causal_forest       3.6 0.00029
#> 2     1.86          0.63         bartc       2.9 0.00326
#> 3     0.87          1.08         lasso       0.8 0.42083
#> 4     1.96          1.12            rf       1.8 0.07940
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation             algorithm statistic p.value
#> 1     0.51          0.68 causal_forest x bartc      0.76    0.45
#> 2     1.51          1.29 causal_forest x lasso      1.17    0.24
#> 3     0.41          0.99    causal_forest x rf      0.42    0.68
#> 4     0.99          0.79         bartc x lasso      1.25    0.21
#> 5    -0.10          0.79            bartc x rf     -0.13    0.90
#> 6    -1.09          1.35            lasso x rf     -0.81    0.42
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     1.24           1.5 causal_forest      0.83    0.41
#> 2     0.77           1.4         bartc      0.54    0.59
#> 3     0.35           1.4         lasso      0.24    0.81
#> 4     1.36           1.5            rf      0.90    0.37
#> 
#> ── GATE ────────────────────────────────────────────────────────────────────────
#>    estimate std.deviation     algorithm group statistic p.value upper lower
#> 1    -124.7            59 causal_forest     1    -2.122   0.034   -93   100
#> 2      94.3            60 causal_forest     2     1.582   0.114   -94   102
#> 3      13.1            59 causal_forest     3     0.222   0.825   -93   101
#> 4       3.0            59 causal_forest     4     0.051   0.960   -93   101
#> 5      32.5            98 causal_forest     5     0.332   0.740  -157   165
#> 6     -14.7            82         bartc     1    -0.180   0.858  -131   138
#> 7    -137.3            87         bartc     2    -1.587   0.113  -139   146
#> 8      54.1            79         bartc     3     0.687   0.492  -126   133
#> 9      85.3            74         bartc     4     1.156   0.248  -118   125
#> 10     30.7            79         bartc     5     0.391   0.696  -126   133
#> 11     71.6            59         lasso     1     1.210   0.226   -94   101
#> 12     -4.1           101         lasso     2    -0.041   0.968  -162   169
#> 13    -95.5            98         lasso     3    -0.973   0.331  -158   165
#> 14     27.2            85         lasso     4     0.319   0.750  -137   144
#> 15     19.0            62         lasso     5     0.303   0.762   -99   106
#> 16      9.8            59            rf     1     0.168   0.867   -93   100
#> 17    -20.1            59            rf     2    -0.338   0.735   -94   101
#> 18    -61.4            59            rf     3    -1.046   0.296   -93   100
#> 19     64.9            59            rf     4     1.101   0.271   -93   101
#> 20     25.0            60            rf     5     0.418   0.676   -95   102
```

We plot the estimated Area Under the Prescriptive Effect Curve for the
writing score across different ML algorithms.

``` r
# plot the AUPEC with different ML algorithms
plot(est_cv)
```

<img src="man/figures/README-multiple_plot-1.png" style="display: block; margin: auto;" />

For caret models, we can extract the training model and check the model
performance.

``` r
# extract the caret model
fit$estimates$models$rf %>% 
  ggplot() + theme_bw() 
```

<img src="man/figures/rf.png" width="100%" />
