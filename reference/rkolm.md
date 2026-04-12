# Random Variates for the Kolmogorov Distribution

Random Variates for the Kolmogorov Distribution

## Usage

``` r
rkolm(n, nterms = 500)
```

## Arguments

- n:

  the number of random variate to simulate

- nterms:

  the number of terms in the limiting sum. That is, turning infinity
  into a Big K on the top of the summation.

## Value

n random variates

## Examples

``` r
## see https://swihart.github.io/mvpd/articles/deep_dive_kolm.html
rkolm(10)
#>            [,1]
#>  [1,] 0.9293508
#>  [2,] 1.0646241
#>  [3,] 0.6198745
#>  [4,] 0.7567944
#>  [5,] 0.6785375
#>  [6,] 0.9683901
#>  [7,] 0.6930647
#>  [8,] 0.7560827
#>  [9,] 0.7415095
#> [10,] 0.6217238
```
