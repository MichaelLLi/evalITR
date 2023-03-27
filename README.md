
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
consider a case with 20% budget constraint (`plim` = 0.2). The model
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

# estimate ITR 
set.seed(2021)
fit <- run_itr(outcome = outcomes,
               treatment = treatment,
               covariates = covariates,
               data = star,
               algorithms = c("causal_forest"),
               plim = 0.2,
               ratio = 0.7)
#> Evaluate ITR under sample splitting ...

# alternatively (with caret package)
# fit <- run_itr(outcome = outcomes,
#                treatment = treatment,
#                covariates = covariates,
#                data = star_df,
#                algorithms = c("caret"),
#                plim = 0.2,
#                ratio = 0.7,
#               trainControl_method = "repeatedcv",
#               train_method = "gbm")

# evaluate ITR 
est <- estimate_itr(fit)
#> Cannot compute PAPDp
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
#> 1     0.78           1.4 causal_forest      0.57    0.57
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      2.8           1.2 causal_forest       2.3   0.024
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#> data frame with 0 columns and 0 rows
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     0.58             1 causal_forest      0.56    0.58
#> 
#> ── GATE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm group statistic p.value upper lower
#> 1     -192           106 causal_forest     1      -1.8   0.070  -168   182
#> 2      184           109 causal_forest     2       1.7   0.092  -173   187
#> 3      227           109 causal_forest     3       2.1   0.038  -172   186
#> 4     -238           107 causal_forest     4      -2.2   0.026  -168   182
#> 5       54           107 causal_forest     5       0.5   0.615  -170   184
```

We plot the estimated Area Under the Prescriptive Effect Curve for the
writing score across a range of budget constraints for causal forest.

``` r
# plot the AUPEC 
plot(est)
```

<img src="man/figures/README-sp_plot-1.png" style="display: block; margin: auto;" />

## Example under cross-validation

The package also allows estimate ITR with k-folds cross-validation.
Instead of specifying the `ratio` argument, we choose the number of
folds (`n_folds`). The following code presents an example of estimating
ITR with 3 folds cross-validation. In practice, we recommend using 10
folds to get a more stable model performance.

``` r
# estimate ITR 
set.seed(2021)
fit_cv <- run_itr(outcome = outcomes,
               treatment = treatment,
               covariates = covariates,
               data = star,
               algorithms = c("causal_forest"),
               plim = 0.2,
               n_folds = 3)
#> Evaluate ITR with cross-validation ...

# evaluate ITR 
est_cv <- estimate_itr(fit_cv)
#> Cannot compute PAPDp
```

We present the results with 3-folds cross validation and plot the AUPEC.

``` r
# summarize estimates
summary(est_cv)
#> ── PAPE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     0.72          0.87 causal_forest      0.82    0.41
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      2.9          0.65 causal_forest       4.4 8.6e-06
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#> data frame with 0 columns and 0 rows
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      1.4           1.5 causal_forest      0.92    0.36
#> 
#> ── GATE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm group statistic p.value upper lower
#> 1     -101            59 causal_forest     1     -1.71   0.087   -93   100
#> 2       37            59 causal_forest     2      0.62   0.536   -94   101
#> 3       39            59 causal_forest     3      0.67   0.506   -93   101
#> 4       14            59 causal_forest     4      0.23   0.816   -94   101
#> 5       29           101 causal_forest     5      0.29   0.773  -162   169
```

``` r
# plot the AUPEC 
plot(est_cv)
```

<img src="man/figures/README-sv_plot-1.png" style="display: block; margin: auto;" />

## Example with multiple ML algorithms/outcomes

We can estimate ITR with various machine learning algorithms and then
compare the performance of each model. The package includes 8 different
ML algorithms (causal forest, BART, lasso, boosting trees, random
forest, CART, bagging trees, svm).

The package also allows estimate heterogeneous treatment effects on the
individual and group-level. On the individual-level, the summary
statistics and the AUPEC plot show whether assigning individualized
treatment rules may outperform complete random experiment. On the
group-level, we specify the number of groups through `ngates` and
estimating heterogeneous treatment effects across groups.

If the original experiment has diverse outcome measures, we develop ITRs
for each outcome and use them to estimate the heterogeneous effects
across the different outcomes.

``` r
# specifying outcomes
outcomes <- c("g3tlangss","g3treadss","g3tmathss")

# specifying covariates
covariates <-  star %>% dplyr::select(-c("g3tlangss","g3treadss","g3tmathss","treatment")) %>% colnames()

# train the model
set.seed(2021)
fit_cv <- run_itr(outcome = outcomes,
               treatment = "treatment",
               covariates = covariates,
               data = star,
               algorithms = c(
                  "causal_forest", 
                  # "bartc",
                  # "svm",
                  "lasso",
                  "boost", 
                  "random_forest",
                  "cart",
                  "bagging"),
               plim = 0.2,
               n_folds = 3)
#> Evaluate ITR with cross-validation ...
#> Evaluate ITR with cross-validation ...
#> Evaluate ITR with cross-validation ...
```

``` r
# compute estimates
est_cv <- estimate_itr(fit_cv)
```

``` r
# summarize estimates
summary(est_cv, outcome = "g3tlangss")
#> ── PAPE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     0.24          1.02 causal_forest      0.24   0.812
#> 2     0.17          1.07         lasso      0.16   0.871
#> 3     0.30          1.35         boost      0.23   0.822
#> 4     2.37          1.01 random_forest      2.34   0.019
#> 5     0.85          0.98          cart      0.87   0.382
#> 6     0.59          0.99       bagging      0.59   0.555
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     2.62          0.65 causal_forest      4.05 5.1e-05
#> 2    -0.20          0.63         lasso     -0.33 7.4e-01
#> 3     1.18          0.63         boost      1.87 6.2e-02
#> 4     0.58          0.84 random_forest      0.69 4.9e-01
#> 5     0.27          0.62          cart      0.44 6.6e-01
#> 6     0.22          1.15       bagging      0.19 8.5e-01
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#>    estimate std.deviation                     algorithm statistic p.value
#> 1     2.824          0.96         causal_forest x lasso     2.937 0.00331
#> 2     1.437          0.75         causal_forest x boost     1.928 0.05391
#> 3     2.045          0.87 causal_forest x random_forest     2.341 0.01925
#> 4     2.347          0.88          causal_forest x cart     2.673 0.00752
#> 5     2.400          0.70       causal_forest x bagging     3.409 0.00065
#> 6    -1.387          1.09                 lasso x boost    -1.275 0.20220
#> 7    -0.779          0.81         lasso x random_forest    -0.965 0.33472
#> 8    -0.478          0.89                  lasso x cart    -0.536 0.59211
#> 9    -0.424          0.88               lasso x bagging    -0.484 0.62841
#> 10    0.608          0.74         boost x random_forest     0.817 0.41401
#> 11    0.910          0.89                  boost x cart     1.024 0.30601
#> 12    0.963          0.68               boost x bagging     1.409 0.15869
#> 13    0.302          0.94          random_forest x cart     0.322 0.74776
#> 14    0.355          0.51       random_forest x bagging     0.695 0.48723
#> 15    0.054          1.19                cart x bagging     0.045 0.96388
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1    1.238           1.5 causal_forest      0.84    0.40
#> 2    0.178           1.4         lasso      0.13    0.90
#> 3    0.309           1.5         boost      0.21    0.83
#> 4    1.294           1.5 random_forest      0.85    0.40
#> 5    0.076           0.7          cart      0.11    0.91
#> 6    0.163           1.4       bagging      0.12    0.91
#> 
#> ── GATE ────────────────────────────────────────────────────────────────────────
#>    estimate std.deviation     algorithm group statistic p.value upper lower
#> 1    -90.56            59 causal_forest     1   -1.5372   0.124   -93   101
#> 2     60.95            59 causal_forest     2    1.0267   0.305   -94   101
#> 3     -7.06            59 causal_forest     3   -0.1196   0.905   -93   101
#> 4     34.23            68 causal_forest     4    0.5066   0.612  -108   115
#> 5     20.65            97 causal_forest     5    0.2125   0.832  -156   164
#> 6    -14.41            94         lasso     1   -0.1539   0.878  -150   158
#> 7    -87.34            88         lasso     2   -0.9943   0.320  -141   148
#> 8     80.72            98         lasso     3    0.8251   0.409  -157   165
#> 9     12.60            59         lasso     4    0.2145   0.830   -93   100
#> 10    26.64            59         lasso     5    0.4512   0.652   -93   101
#> 11     0.73            82         boost     1    0.0089   0.993  -132   139
#> 12   -33.78            59         boost     2   -0.5691   0.569   -94   101
#> 13   -44.09            84         boost     3   -0.5231   0.601  -135   142
#> 14    81.86            96         boost     4    0.8510   0.395  -155   162
#> 15    13.48            59         boost     5    0.2279   0.820   -94   101
#> 16   -12.61            59 random_forest     1   -0.2134   0.831   -94   101
#> 17    36.26            87 random_forest     2    0.4161   0.677  -140   147
#> 18   -94.79            97 random_forest     3   -0.9807   0.327  -155   163
#> 19    78.42            59 random_forest     4    1.3344   0.182   -93   100
#> 20    10.94            59 random_forest     5    0.1844   0.854   -94   101
#> 21    38.85            78          cart     1    0.4951   0.621  -125   133
#> 22    21.94            59          cart     2    0.3724   0.710   -93   101
#> 23   -77.78            92          cart     3   -0.8433   0.399  -148   155
#> 24   -20.95            92          cart     4   -0.2278   0.820  -148   155
#> 25    56.15            98          cart     5    0.5746   0.566  -157   164
#> 26    63.37            59       bagging     1    1.0694   0.285   -94   101
#> 27   -43.95            94       bagging     2   -0.4679   0.640  -151   158
#> 28   -39.78           102       bagging     3   -0.3895   0.697  -164   172
#> 29    97.10            59       bagging     4    1.6551   0.098   -93   100
#> 30   -58.53            59       bagging     5   -0.9882   0.323   -94   101
```

We plot the estimated Area Under the Prescriptive Effect Curve for the
writing score across different ML algorithms.

``` r
# plot the AUPEC with different ML algorithms
plot(est_cv, outcome = "g3tlangss")
```

<img src="man/figures/README-multiple_plot-1.png" style="display: block; margin: auto;" />
