
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
#> 1      2.4          0.89 causal_forest       2.7  0.0072
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      4.1           1.3 causal_forest       3.1   0.002
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#> data frame with 0 columns and 0 rows
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1      2.9          0.91 causal_forest       3.2  0.0014
#> 
#> ── GATE ────────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm group statistic p.value upper lower
#> 1       47           108 causal_forest     1      0.43    0.66  -175   182
#> 2       12           108 causal_forest     2      0.11    0.91  -174   181
#> 3      -24           109 causal_forest     3     -0.22    0.83  -176   182
#> 4      -64           109 causal_forest     4     -0.59    0.56  -175   182
#> 5       44           108 causal_forest     5      0.40    0.69  -175   181

# similarly for caret
# summary(est_caret)
```

We can extract estimates from the `est` object. The following code shows
how to extract the GATE estimates for the writing score with `rlasso`
and `lasso` algorithms.

``` r
# plot GATE estimates
summary(est)$GATE %>%
  mutate(conf.low = estimate - 1.96 * std.deviation,
         conf.high = estimate + 1.96 * std.deviation,
         group = as_factor(group)) %>%
  ggplot(., aes(
    x = group, y = estimate,
    ymin = conf.low , ymax = conf.high, color = algorithm)) +
  ggdist::geom_pointinterval(
    width=0.5,    
    position=position_dodge(0.5),
    interval_size_range = c(0.8, 1.5),
    fatten_point = 2.5) +
  theme_bw() +    
  theme(panel.grid element_blank(),
        panel.background = element_blank()) +
  labs(x = "Group", y = "GATE estimate") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#4e4e4e") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#076f00", "#0072B2")) 
```

<img src="man/figures/gate.png" width="60%" />

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
#> 1    0.954          0.82 causal_forest      1.17    0.24
#> 2   -0.076          0.46         bartc     -0.16    0.87
#> 3    0.173          1.07         lasso      0.16    0.87
#> 4    1.266          0.95            rf      1.33    0.18
#> 
#> ── PAPEp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     2.55          0.65 causal_forest      3.91 9.2e-05
#> 2     1.38          0.76         bartc      1.81 7.0e-02
#> 3    -0.21          0.63         lasso     -0.33 7.4e-01
#> 4     1.69          1.11            rf      1.52 1.3e-01
#> 
#> ── PAPDp ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation             algorithm statistic p.value
#> 1     1.18          0.95 causal_forest x bartc       1.2 0.21608
#> 2     2.76          0.80 causal_forest x lasso       3.5 0.00054
#> 3     0.87          0.71    causal_forest x rf       1.2 0.22292
#> 4     1.58          1.06         bartc x lasso       1.5 0.13651
#> 5    -0.31          1.01            bartc x rf      -0.3 0.76073
#> 6    -1.89          0.72            lasso x rf      -2.6 0.00892
#> 
#> ── AUPEC ───────────────────────────────────────────────────────────────────────
#>   estimate std.deviation     algorithm statistic p.value
#> 1     1.43           1.5 causal_forest      0.92    0.36
#> 2     0.76           1.4         bartc      0.55    0.58
#> 3     0.18           1.4         lasso      0.13    0.90
#> 4     1.37           1.6            rf      0.88    0.38
#> 
#> ── GATE ────────────────────────────────────────────────────────────────────────
#>    estimate std.deviation     algorithm group statistic p.value upper lower
#> 1    -118.1            59 causal_forest     1    -2.013   0.044   -93   100
#> 2      27.0            59 causal_forest     2     0.454   0.650   -94   101
#> 3      60.9            59 causal_forest     3     1.034   0.301   -93   101
#> 4       7.6            59 causal_forest     4     0.128   0.898   -94   101
#> 5      40.9            99 causal_forest     5     0.411   0.681  -160   167
#> 6      -7.8            74         bartc     1    -0.105   0.916  -118   125
#> 7    -107.9            59         bartc     2    -1.823   0.068   -94   101
#> 8      62.8            94         bartc     3     0.668   0.504  -151   158
#> 9       9.4            59         bartc     4     0.159   0.874   -94   101
#> 10     61.7            99         bartc     5     0.625   0.532  -159   166
#> 11    -14.4            94         lasso     1    -0.154   0.878  -150   158
#> 12    -94.5            90         lasso     2    -1.051   0.293  -144   152
#> 13     87.9            99         lasso     3     0.886   0.376  -160   167
#> 14     12.6            59         lasso     4     0.214   0.830   -93   100
#> 15     26.6            59         lasso     5     0.451   0.652   -93   101
#> 16    -37.4            59            rf     1    -0.638   0.523   -93   100
#> 17     10.6            59            rf     2     0.180   0.857   -94   101
#> 18    -17.6            59            rf     3    -0.299   0.765   -93   100
#> 19     66.5            86            rf     4     0.770   0.441  -139   146
#> 20     -3.9            60            rf     5    -0.066   0.948   -94   102
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
