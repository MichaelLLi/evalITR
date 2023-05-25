
<!-- README.md is generated from README.Rmd. Please edit that file -->

## evalITR [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/evalITR)](https://cran.r-project.org/package=evalITR) ![CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/evalITR)

<!-- badges: start -->
<!-- badges: end -->

![](man/figures/README-manual.png)<!-- -->

R package evalITR provides various statistical methods for estimating
and evaluating Individualized Treatment Rules under randomized data. The
provided metrics include (1) population average prescriptive effect
`PAPE`; (2) population average prescriptive effect with a budget
constraint `PAPEp`; (3) population average prescriptive effect
difference with a budget constraint `PAPDp`. This quantity will be
computed with more than 2 machine learning algorithms); (4) and area
under the prescriptive effect curve `AUPEC`. For more information about
these evaluation metrics, please refer to [Imai and Li
(2021)](https://arxiv.org/abs/1905.05389); (5) Grouped Average Treatment
Effects `GATEs`. The details of the methods for this design are given in
[Imai and Li (2022)](https://arxiv.org/abs/2203.14511).
