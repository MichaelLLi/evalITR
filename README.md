
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
train the model, while the remaining 30% is used as testing data (ratio
= 0.7).

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
fit <- run_itr(outcome = outcomes,
               treatment = treatment,
               covariates = covariates,
               data = star,
               algorithms = c("causal_forest"),
               plim = 0.2,
               ratio = 0.7)
#> Evaluate ITR under sample splitting ...

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
#> 1     0.56           1.4 causal_forest       0.4    0.69
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      1.4           1.2 causal_forest       1.1    0.25
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#> data frame with 0 columns and 0 rows
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      1.2           1.1 causal_forest       1.2    0.24
#> 
#> ── GATE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm group statistic p.value upper lower
#> 1      -76           108 causal_forest     1     -0.70   0.483  -172   182
#> 2     -219           107 causal_forest     2     -2.03   0.042  -171   182
#> 3      160           108 causal_forest     3      1.49   0.137  -172   183
#> 4      106           109 causal_forest     4      0.97   0.333  -175   185
#> 5       55           107 causal_forest     5      0.51   0.608  -171   181
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
#> 1      1.1           1.3 causal_forest      0.89    0.37
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      1.3          0.69 causal_forest       1.9   0.062
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#> data frame with 0 columns and 0 rows
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      1.4          0.67 causal_forest       2.1   0.033
#> 
#> ── GATE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm group statistic p.value upper lower
#> 1       12            59 causal_forest     1      0.21    0.84   -93   101
#> 2      -47            85 causal_forest     2     -0.55    0.58  -135   143
#> 3       18            59 causal_forest     3      0.31    0.76   -94   101
#> 4      -61           101 causal_forest     4     -0.61    0.55  -162   169
#> 5       95           100 causal_forest     5      0.95    0.34  -162   169
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
#> Evaluate ITR with cross-validation ...Evaluate ITR with cross-validation ...Evaluate ITR with cross-validation ...
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
#> 1     0.88          1.08 causal_forest      0.81  0.4155
#> 2     0.25          0.84         lasso      0.29  0.7696
#> 3     2.24          0.87         boost      2.57  0.0102
#> 4     2.28          0.80 random_forest      2.86  0.0043
#> 5     1.24          0.92          cart      1.34  0.1790
#> 6     1.86          1.26       bagging      1.48  0.1383
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     3.17          0.74 causal_forest      4.29 1.8e-05
#> 2     0.95          0.99         lasso      0.96 3.4e-01
#> 3     1.67          0.79         boost      2.11 3.5e-02
#> 4     1.93          0.78 random_forest      2.48 1.3e-02
#> 5    -0.15          0.88          cart     -0.17 8.7e-01
#> 6     1.41          0.78       bagging      1.80 7.1e-02
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#>    estimate std.deviation                     algorithm statistic p.value
#> 1      2.22          0.93         causal_forest x lasso      2.38   0.017
#> 2      1.50          0.75         causal_forest x boost      1.98   0.047
#> 3      1.24          0.62 causal_forest x random_forest      2.00   0.045
#> 4      3.31          1.35          causal_forest x cart      2.46   0.014
#> 5      1.76          0.71       causal_forest x bagging      2.47   0.014
#> 6     -0.72          0.73                 lasso x boost     -0.99   0.320
#> 7     -0.98          1.20         lasso x random_forest     -0.82   0.411
#> 8      1.09          1.02                  lasso x cart      1.07   0.283
#> 9     -0.46          0.81               lasso x bagging     -0.57   0.570
#> 10    -0.26          0.76         boost x random_forest     -0.34   0.732
#> 11     1.82          1.57                  boost x cart      1.16   0.248
#> 12     0.26          1.09               boost x bagging      0.24   0.810
#> 13     2.08          0.96          random_forest x cart      2.16   0.031
#> 14     0.52          0.50       random_forest x bagging      1.04   0.298
#> 15    -1.55          1.21                cart x bagging     -1.29   0.197
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     1.63           2.1 causal_forest      0.77    0.44
#> 2     0.21           2.1         lasso      0.10    0.92
#> 3     1.58           1.4         boost      1.13    0.26
#> 4     1.87           2.1 random_forest      0.91    0.36
#> 5     0.32           1.3          cart      0.25    0.80
#> 6     1.44           1.6       bagging      0.91    0.36
#> 
#> ── GATE ────────────────────────────────────────────────────────────────────────
#>    estimate std.deviation     algorithm group statistic p.value upper lower
#> 1    -58.28           102 causal_forest     1   -0.5728    0.57  -164   171
#> 2     25.77            65 causal_forest     2    0.3995    0.69  -103   110
#> 3      0.42            96 causal_forest     3    0.0044    1.00  -154   161
#> 4     13.23            91 causal_forest     4    0.1454    0.88  -146   153
#> 5     36.75            82 causal_forest     5    0.4468    0.66  -132   139
#> 6     70.69            60         lasso     1    1.1850    0.24   -95   102
#> 7    -60.90            91         lasso     2   -0.6728    0.50  -145   152
#> 8     31.06            82         lasso     3    0.3773    0.71  -132   139
#> 9     18.39            70         lasso     4    0.2625    0.79  -112   119
#> 10   -41.34            59         lasso     5   -0.7021    0.48   -93   100
#> 11   -43.17           101         boost     1   -0.4263    0.67  -163   170
#> 12    32.56            99         boost     2    0.3305    0.74  -158   166
#> 13    24.62            61         boost     3    0.4046    0.69   -97   104
#> 14   -18.88            74         boost     4   -0.2538    0.80  -119   126
#> 15    22.75            71         boost     5    0.3195    0.75  -114   121
#> 16    18.73           100 random_forest     1    0.1877    0.85  -160   168
#> 17   -42.93           102 random_forest     2   -0.4212    0.67  -164   171
#> 18   -43.44            97 random_forest     3   -0.4457    0.66  -157   164
#> 19    34.48            59 random_forest     4    0.5846    0.56   -93   101
#> 20    51.05            69 random_forest     5    0.7453    0.46  -109   116
#> 21    11.35            70          cart     1    0.1615    0.87  -112   119
#> 22    63.04            86          cart     2    0.7312    0.46  -138   145
#> 23  -111.37            90          cart     3   -1.2328    0.22  -145   152
#> 24   115.84            74          cart     4    1.5710    0.12  -118   125
#> 25   -60.97            91          cart     5   -0.6721    0.50  -146   153
#> 26   -40.71            92       bagging     1   -0.4436    0.66  -147   155
#> 27    13.76            68       bagging     2    0.2027    0.84  -108   115
#> 28   -23.53            81       bagging     3   -0.2905    0.77  -130   137
#> 29    73.95           100       bagging     4    0.7364    0.46  -162   169
#> 30    -5.58            65       bagging     5   -0.0865    0.93  -103   110
```

We plot the estimated Area Under the Prescriptive Effect Curve for the
writing score across different ML algorithms.

``` r
# plot the AUPEC with different ML algorithms
plot(est_cv, outcome = "g3tlangss")
```

<img src="man/figures/README-multiple_plot-1.png" style="display: block; margin: auto;" />
