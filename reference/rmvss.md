# Multivariate Subgaussian Stable Random Variates

Computes random vectors of the multivariate subgaussian stable
distribution for arbitrary alpha, shape matrices, and location vectors.
See Nolan (2013).

## Usage

``` r
rmvss(
  n,
  alpha = 1,
  Q = NULL,
  delta = rep(0, d),
  which.stable = c("libstable4u", "stabledist")[1]
)
```

## Arguments

- n:

  number of observations

- alpha:

  default to 1 (Cauchy). Must be 0\<`alpha`\<2

- Q:

  Shape matrix. See Nolan (2013).

- delta:

  location vector.

- which.stable:

  defaults to `"libstable4u"`, other option is `"stabledist"`. Indicates
  which package should provide the univariate stable distribution in
  this production distribution form of a univariate stable and
  multivariate normal.

## Value

Returns the `n` by `d` matrix containing multivariate subgaussian stable
random variates where `d=nrow(Q)`.

## References

Nolan JP (2013), *Multivariate elliptically contoured stable
distributions: theory and estimation*. Comput Stat (2013) 28:2067–2089
DOI 10.1007/s00180-013-0396-7

## Examples

``` r
## generate 10 random variates of a bivariate mvss
rmvss(n=10, alpha=1.71, Q=matrix(c(10,7.5,7.5,10),2))
#>               [,1]       [,2]
#>  [1,] -5.779844757  0.6433924
#>  [2,]  5.503686323  2.1582754
#>  [3,] -1.452248919 -0.1811842
#>  [4,]  0.007002745 -0.4660899
#>  [5,] -8.965921190 -7.8356139
#>  [6,]  3.224200045  4.4361373
#>  [7,] -3.408008189 -9.1435620
#>  [8,] -1.855945807  2.0053971
#>  [9,]  5.757703456  4.9498872
#> [10,] -0.726099987  4.9042458

## generate 10 random variates of a trivariate mvss
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
rmvss(n=10, alpha=1.71, Q=Q)
#>             [,1]        [,2]       [,3]
#>  [1,] -0.1848501  1.35918277  0.4681466
#>  [2,] -0.7494501  1.64826613  1.5725410
#>  [3,] -1.4719466  1.60423095  0.1485459
#>  [4,] -8.2181965 -0.01225778 -5.2020751
#>  [5,] -4.7699976  2.29265649 -3.3422922
#>  [6,] -0.7451184 -1.28517677 -2.2209513
#>  [7,] -0.3303047 -2.72379987 -0.7226760
#>  [8,]  2.1008274  3.87670850  2.7713000
#>  [9,]  4.2320237  1.95445116  4.3933210
#> [10,] -3.0476230 -6.02700635 -3.7677116

```
