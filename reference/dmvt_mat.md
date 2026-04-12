# Multivariate t-Distribution Density for matrix inputs

Computes the the density function of the multivariate subgaussian stable
distribution for arbitrary degrees of freedom, shape matrices, and
location vectors. See Swihart and Nolan (2022).

## Usage

``` r
dmvt_mat(
  x,
  df = 1,
  Q = NULL,
  delta = rep(0, d),
  outermost.int = c("stats::integrate", "cubature::adaptIntegrate",
    "cubature::hcubature")[2],
  spherical = FALSE,
  subdivisions.si = 100L,
  rel.tol.si = .Machine$double.eps^0.25,
  abs.tol.si = rel.tol.si,
  stop.on.error.si = TRUE,
  tol.ai = 1e-05,
  fDim.ai = NULL,
  maxEval.ai = 0,
  absError.ai = 0,
  doChecking.ai = FALSE
)
```

## Arguments

- x:

  nxd matrix of n variates of d-dimension

- df:

  default to 1 (Cauchy). This is df for t-dist, real number df\>0. Can
  be non-integer.

- Q:

  Shape matrix. See Nolan (2013).

- delta:

  location vector

- outermost.int:

  select which integration function to use for outermost integral.
  Default is "stats::integrate" and one can specify the following
  options with the `.si` suffix. For diagonal Q, one can also specify
  "cubature::adaptIntegrate" and use the `.ai` suffix options below
  (currently there is a bug for non-diagnoal Q).

- spherical:

  default is FALSE. If true, use the spherical transformation. Results
  will be identical to spherical = FALSE but may be faster.

- subdivisions.si:

  the maximum number of subintervals. The suffix `.si` indicates a
  [`stats::integrate()`](https://rdrr.io/r/stats/integrate.html) option
  for the outermost semi-infinite integral in the product distribution
  formulation.

- rel.tol.si:

  relative accuracy requested. The suffix `.si` indicates a
  [`stats::integrate()`](https://rdrr.io/r/stats/integrate.html) option
  for the outermost semi-infinite integral in the product distribution
  formulation.

- abs.tol.si:

  absolute accuracy requested. The suffix `.si` indicates a
  [`stats::integrate()`](https://rdrr.io/r/stats/integrate.html) option
  for the outermost semi-infinite integral in the product distribution
  formulation.

- stop.on.error.si:

  logical. If true (the default) an error stops the function. If false
  some errors will give a result with a warning in the message
  component. The suffix `.si` indicates a
  [`stats::integrate()`](https://rdrr.io/r/stats/integrate.html) option
  for the outermost semi-infinite integral in the product distribution
  formulation.

- tol.ai:

  The maximum tolerance, default 1e-5. The suffix `.ai` indicates a
  [`cubature::adaptIntegrate`](https://bnaras.github.io/cubature/reference/hcubature.html)
  type option for the outermost semi-infinite integral in the product
  distribution formulation.

- fDim.ai:

  The dimension of the integrand, default 1, bears no relation to the
  dimension of the hypercube The suffix `.ai` indicates a
  [`cubature::adaptIntegrate`](https://bnaras.github.io/cubature/reference/hcubature.html)
  type option for the outermost semi-infinite integral in the product
  distribution formulation.

- maxEval.ai:

  The maximum number of function evaluations needed, default 0 implying
  no limit The suffix `.ai` indicates a
  [`cubature::adaptIntegrate`](https://bnaras.github.io/cubature/reference/hcubature.html)
  type option for the outermost semi-infinite integral in the product
  distribution formulation.

- absError.ai:

  The maximum absolute error tolerated The suffix `.ai` indicates a
  [`cubature::adaptIntegrate`](https://bnaras.github.io/cubature/reference/hcubature.html)
  type option for the outermost semi-infinite integral in the product
  distribution formulation.

- doChecking.ai:

  A flag to be thorough checking inputs to C routines. A FALSE value
  results in approximately 9 percent speed gain in our experiments. Your
  mileage will of course vary. Default value is FALSE. The suffix `.ai`
  indicates a
  [`cubature::adaptIntegrate`](https://bnaras.github.io/cubature/reference/hcubature.html)
  type option for the outermost semi-infinite integral in the product
  distribution formulation.

## Value

The object returned depends on what is selected for `outermost.int`. In
the case of the default,
[`stats::integrate`](https://rdrr.io/r/stats/integrate.html), the value
is a list of class "integrate" with components:

- `value`:

  the final estimate of the integral.

- `abs.error`:

  estimate of the modulus of the absolute error.

- `subdivisions`:

  the number of subintervals produced in the subdivision process.

- `message`:

  "OK" or a character string giving the error message.

- `call`:

  the matched call.

Note: The reported `abs.error` is likely an under-estimate as
`integrate` assumes the integrand was without error, which is not the
case in this application.

## References

Swihart & Nolan, "The R Journal: Multivariate Subgaussian Stable
Distributions in R", The R Journal, 2022

## Examples

``` r
x <- c(1.23, 4.56)
mu <- 1:2
Sigma <- matrix(c(4, 2, 2, 3), ncol=2)
df01 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  1, log=FALSE) # default log = TRUE!
df10 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df = 10, log=FALSE) # default log = TRUE!
df30 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df = 30, log=FALSE) # default log = TRUE!
df01
#> [1] 0.007027824
df10
#> [1] 0.01164573
df30
#> [1] 0.01223266


dmvt_mat(
  matrix(x,ncol=2),
  df = 1,
  Q = Sigma,
  delta=mu)$int
#> [1] 0.007027824


dmvt_mat(
  matrix(x,ncol=2),
  df = 10,
  Q = Sigma,
  delta=mu)$int
#> [1] 0.01164573


dmvt_mat(
  matrix(x,ncol=2),
  df = 30,
  Q = Sigma,
  delta=mu)$int
#> [1] 0.01223266

## Q: can we do non-integer degrees of freedom?
## A: yes for both mvpd::dmvt_mat and mvtnorm::dmvt

df1.5 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  1.5, log=FALSE) # default log = TRUE!
df1.5
#> [1] 0.008221199

dmvt_mat(
  matrix(x,ncol=2),
  df = 1.5,
  Q = Sigma,
  delta=mu)$int
#> [1] 0.008221199


## Q: can we do <1 degrees of freedom but >0?
## A: yes for both mvpd::dmvt_mat and mvtnorm::dmvt

df0.5 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  0.5, log=FALSE) # default log = TRUE!
df0.5
#> [1] 0.004938052

dmvt_mat(
  matrix(x,ncol=2),
  df = 0.5,
  Q = Sigma,
  delta=mu)$int
#> [1] 0.004938052

df0.0001 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  0.0001, 
                          log=FALSE) # default log = TRUE!
df0.0001
#> [1] 1.873233e-06

dmvt_mat(
  matrix(x,ncol=2),
  df = 0.0001,
  Q = Sigma,
  delta=mu)$int
#> [1] 1.873233e-06



## Q: can we do ==0 degrees of freedom?
## A: No for both mvpd::dmvt_mat and mvtnorm::dmvt

## this just becomes normal, as per the manual for mvtnorm::dmvt
df0.0 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  0, log=FALSE) # default log = TRUE!
df0.0
#> [1] 0.01254144

if (FALSE) { # \dontrun{
dmvt_mat(
  matrix(x,ncol=2),
  df = 0,
  Q = Sigma,
  delta=mu)$int 
} # }
```
