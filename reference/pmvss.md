# Multivariate Subgaussian Stable Distribution

Computes the probabilities for the multivariate subgaussian stable
distribution for arbitrary limits, alpha, shape matrices, and location
vectors. See Nolan (2013).

## Usage

``` r
pmvss(
  lower = rep(-Inf, d),
  upper = rep(Inf, d),
  alpha = 1,
  Q = NULL,
  delta = rep(0, d),
  maxpts.pmvnorm = 25000,
  abseps.pmvnorm = 0.001,
  outermost.int = c("stats::integrate", "cubature::adaptIntegrate")[1],
  subdivisions.si = 100L,
  rel.tol.si = .Machine$double.eps^0.25,
  abs.tol.si = rel.tol.si,
  stop.on.error.si = TRUE,
  tol.ai = 1e-05,
  fDim.ai = 1,
  maxEval.ai = 0,
  absError.ai = 0,
  doChecking.ai = FALSE,
  which.stable = c("libstable4u", "stabledist")[1]
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

- maxpts.pmvnorm:

  Defaults to 25000. Passed to the F_G = pmvnorm() in the integrand of
  the outermost integral.

- abseps.pmvnorm:

  Defaults to 1e-3. Passed to the F_G = pmvnorm() in the integrand of
  the outermost integral.

- outermost.int:

  select which integration function to use for outermost integral.
  Default is "stats::integrate" and one can specify the following
  options with the `.si` suffix. For diagonal Q, one can also specify
  "cubature::adaptIntegrate" and use the `.ai` suffix options below
  (currently there is a bug for non-diagonal Q).

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

- which.stable:

  defaults to "libstable4u", other option is "stabledist". Indicates
  which package should provide the univariate stable distribution in
  this production distribution form of a univariate stable and
  multivariate normal.

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

Nolan JP (2013), *Multivariate elliptically contoured stable
distributions: theory and estimation*. Comput Stat (2013) 28:2067–2089
DOI 10.1007/s00180-013-0396-7

## Examples

``` r
## bivariate
U <- c(1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,10),2)
mvpd::pmvss(L, U, alpha=1.71, Q=Q)
#> 0.04973221 with absolute error < 4.2e-05

## trivariate
U <- c(1,1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
mvpd::pmvss(L, U, alpha=1.71, Q=Q)
#> 0.01591206 with absolute error < 1.8e-05

## How `delta` works: same as centering
U <- c(1,1,1)
L <- -U
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
D <- c(0.75, 0.65, -0.35)
mvpd::pmvss(L-D, U-D, alpha=1.71, Q=Q)
#> 0.01446931 with absolute error < 0.00011
mvpd::pmvss(L  , U  , alpha=1.71, Q=Q, delta=D)
#> 0.01446929 with absolute error < 0.00011


```
