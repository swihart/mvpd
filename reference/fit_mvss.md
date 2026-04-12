# Fit a Multivariate Subgaussian Distribution

Estimates the parameters (namely, alpha, shape matrix Q, and location
vector) of the multivariate subgaussian distribution for an input matrix
X.

## Usage

``` r
fit_mvss(x)
```

## Arguments

- x:

  a matrix for which the parameters for a `d`-dimensional multivariate
  subgaussian distribution will be estimated. The number of columns will
  be `d`.

## Value

A list with parameters from the column-wise univariate fits and the
multivariate alpha and shape matrix estimates (the `univ_deltas` are the
`mult_deltas`):

- `univ_alphas` - the alphas from the column-wise univariate fits

- `univ_betas` - the betas from the column-wise univariate fits

- `univ_gammas` - the gammas from the column-wise univariate fits

- `univ_deltas` - the deltas from the column-wise univariate fits

- `mult_alpha` - the mean(univ_alphas); equivalently the multivariate
  alpha estimate

- `mult_Q_raw` - the multivariate shape matrix estimate (before applying
  `nearPD()`)

- `mult_Q_posdef` - the nearest positive definite multivariate shape
  matrix estimate, `nearPD(mult_Q_raw)`

## Details

Using the protocols outlined in Nolan (2013), this function uses
`libstable4u`'s univariate fit functions for each component.

## References

Nolan JP (2013), *Multivariate elliptically contoured stable
distributions: theory and estimation*. Comput Stat (2013) 28:2067–2089
DOI 10.1007/s00180-013-0396-7

## See also

`Rfast::mvnorm.mle`, `alphastable::mfitstab.elliptical`

## Examples

``` r
# \donttest{
## create a 4x4 shape matrix symMat
S <- matrix(rnorm(4*4, mean=2, sd=4),4); 
symMat <- as.matrix(Matrix::nearPD(0.5 * (S + t(S)))$mat)
symMat
#>            [,1]      [,2]       [,3]       [,4]
#> [1,]  1.4405616  2.581618 -0.5364308  0.4824483
#> [2,]  2.5816179  6.898871 -2.7719604 -0.5159260
#> [3,] -0.5364308 -2.771960  1.6424608  0.9203432
#> [4,]  0.4824483 -0.515926  0.9203432  1.0002679
## generate 10,000 r.v.'s from 4-dimensional mvss
X <- mvpd::rmvss(1e4, alpha=1.5, Q=symMat, delta=c(1,2,3,4))
## use fit_mvss to recover the parameters, compare to symMat
fmv <- mvpd::fit_mvss(X)
fmv
#> $univ_alphas
#> [1] 1.513308 1.537871 1.536045 1.518953
#> 
#> $univ_betas
#> [1]  0.001161657  0.008100327 -0.025793228  0.004121532
#> 
#> $univ_gammas
#> [1] 1.2004493 2.6457106 1.3009471 0.9991121
#> 
#> $univ_deltas
#> [1] 0.9553778 1.9263515 2.9844314 3.9725634
#> 
#> $mult_alpha
#> [1] 1.526544
#> 
#> $mult_Q_raw
#>            [,1]       [,2]       [,3]       [,4]
#> [1,]  1.4410786  2.6832637 -0.5723522  0.4562007
#> [2,]  2.6832637  6.9997844 -2.8527823 -0.5335773
#> [3,] -0.5723522 -2.8527823  1.6924633  0.9081656
#> [4,]  0.4562007 -0.5335773  0.9081656  0.9982249
#> 
#> $mult_Q_posdef
#>            [,1]       [,2]       [,3]       [,4]
#> [1,]  1.4673593  2.6667974 -0.5869613  0.4490156
#> [2,]  2.6667974  7.0101013 -2.8436289 -0.5290755
#> [3,] -0.5869613 -2.8436289  1.7005843  0.9121597
#> [4,]  0.4490156 -0.5290755  0.9121597  1.0001893
#> 
symMat
#>            [,1]      [,2]       [,3]       [,4]
#> [1,]  1.4405616  2.581618 -0.5364308  0.4824483
#> [2,]  2.5816179  6.898871 -2.7719604 -0.5159260
#> [3,] -0.5364308 -2.771960  1.6424608  0.9203432
#> [4,]  0.4824483 -0.515926  0.9203432  1.0002679
## then use the fitted parameters to calculate a probability:
mvpd::pmvss(lower=rep(0,4),
            upper=rep(5,4),
            alpha=fmv$mult_alpha,
            Q=fmv$mult_Q_posdef,
            delta=fmv$univ_deltas,
            maxpts.pmvnorm = 25000*10)
#> 0.3194962 with absolute error < 9.9e-06
# }
```
