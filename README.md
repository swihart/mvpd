
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mvpd

<!-- badges: start -->
<!-- badges: end -->

The goal of mvpd is to …

## Installation

You can install the development version of mvpd from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("swihart/mvpd")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mvpd)
#> Loading required package: mvtnorm
#> Loading required package: stabledist
#> Loading required package: libstableR
#> Loading required package: cubature
#> Loading required package: matrixStats
## basic example code
rmvss(n=10, alpha=1.71, Q=matrix(c(10,7.5,7.5,10),2))
#>             [,1]       [,2]
#>  [1,]  0.4830303  0.8441796
#>  [2,]  5.1963038  0.6223641
#>  [3,]  5.4018070  1.0948431
#>  [4,]  4.1411807 -2.9712771
#>  [5,] -4.7835361 -0.4984347
#>  [6,]  0.4454276  3.5046986
#>  [7,]  4.5602384 -5.2487251
#>  [8,] -2.5577414 -0.4614110
#>  [9,]  0.5624298  1.0798859
#> [10,] -1.4641572 -0.7514232
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
