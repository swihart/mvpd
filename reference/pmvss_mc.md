# Monte Carlo Multivariate Subgaussian Stable Distribution

Computes probabilities of the multivariate subgaussian stable
distribution for arbitrary limits, alpha, shape matrices, and location
vectors via Monte Carlo (thus the suffix `_mc`).

## Usage

``` r
pmvss_mc(
  lower = rep(-Inf, d),
  upper = rep(Inf, d),
  alpha = 1,
  Q = NULL,
  delta = rep(0, d),
  which.stable = c("libstable4u", "stabledist")[1],
  n = NULL
)
```

## Arguments

- lower:

  the vector of lower limits of length n.

- upper:

  the vector of upper limits of length n.

- alpha:

  default to 1 (Cauchy). Must be 0\<alpha\<2

- Q:

  Shape matrix. See Nolan (2013).

- delta:

  location vector.

- which.stable:

  defaults to "libstable4u", other option is "stabledist". Indicates
  which package should provide the univariate stable distribution in
  this production distribution form of a univariate stable and
  multivariate normal.

- n:

  number of random vectors to be drawn for Monte Carlo calculation.

## Value

a number between 0 and 1, the estimated probability via Monte Carlo

## References

Nolan JP (2013), *Multivariate elliptically contoured stable
distributions: theory and estimation*. Comput Stat (2013) 28:2067–2089
DOI 10.1007/s00180-013-0396-7

## Examples

``` r
## print("mvpd (d=2, alpha=1.71):")
U <- c(1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,10),2)
mvpd::pmvss_mc(L, U, alpha=1.71, Q=Q, n=1e3)
#> [1] 0.051
mvpd::pmvss   (L, U, alpha=1.71, Q=Q)
#> 0.04973221 with absolute error < 4.2e-05

## more accuracy = longer runtime
mvpd::pmvss_mc(L, U, alpha=1.71, Q=Q, n=1e4)
#> [1] 0.0494

U <- c(1,1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
## print("mvpd: (d=3, alpha=1.71):")
mvpd::pmvss_mc(L, U, alpha=1.71, Q=Q, n=1e3)
#> [1] 0.017

```
