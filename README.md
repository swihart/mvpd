
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mvpd: An R Package for Multivariate Product Distributions

-   `[dpr]mvss`: mutivariate subgaussian stable distributions

<!-- badges: start -->
<!-- badges: end -->

The goal of `mvpd` is to use product distribution theory to allow the
numerical calculations of specific scale mixtures of the multivariate
normal distribution. The multivariate subgaussian stable distribution is
the product of the square root of a univariate positive stable
distribution and the multivariate normal distribution (see Nolan
(2013)).

## Installation

You can install the development version of mvpd from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("swihart/mvpd")
```

## Example

Generate 1000 draws from a random bivariate subgaussian stable
distribution with alpha=1.71 and plot.

``` r
library(mvpd)
## basic example code
biv <- rmvss(n=1e3, alpha=1.71, Q=matrix(c(10,7.5,7.5,10),2))
head(biv)
#>             [,1]       [,2]
#> [1,]   0.3074873   1.058549
#> [2,]   0.5291253   3.329567
#> [3,] -10.2487185 -16.424006
#> [4,]  13.9380862  20.285716
#> [5,]   5.2389706   5.971667
#> [6,]  -2.3023545  -3.261889
plot(biv); abline(h=0,v=0)
```

<img src="man/figures/README-example-1.png" width="100%" />
