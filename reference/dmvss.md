# Multivariate Subgaussian Stable Density

Computes the the density function of the multivariate subgaussian stable
distribution for arbitrary alpha, shape matrices, and location vectors.
See Nolan (2013).

## Usage

``` r
dmvss(
  x,
  alpha = 1,
  Q = NULL,
  delta = rep(0, d),
  outermost.int = c("stats::integrate", "cubature::adaptIntegrate")[1],
  spherical = FALSE,
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

- x:

  vector of quantiles.

- alpha:

  default to 1 (Cauchy). Must be 0\<alpha\<2

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

Nolan, John P. "Multivariate elliptically contoured stable
distributions: theory and estimation." Computational Statistics 28.5
(2013): 2067-2089.

## Examples

``` r
## print("mvsubgaussPD (d=2, alpha=1.71):")
Q <- matrix(c(10,7.5,7.5,10),2)
mvpd::dmvss(x=c(0,1), alpha=1.71, Q=Q)
#> 0.01211828 with absolute error < 5.8e-05

## more accuracy = longer runtime
mvpd::dmvss(x=c(0,1),alpha=1.71, Q=Q, abs.tol=1e-8)
#> 0.01211828 with absolute error < 5.7e-07

Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
## print("mvsubgausPD (d=3, alpha=1.71):")
mvpd::dmvss(x=c(0,1,2), alpha=1.71, Q=Q)
#> 0.001602922 with absolute error < 4.4e-05
mvpd::dmvss(x=c(0,1,2), alpha=1.71, Q=Q, spherical=TRUE)
#> 0.001602922 with absolute error < 4.4e-05

## How `delta` works: same as centering
X <- c(1,1,1)
Q <- matrix(c(10,7.5,7.5,7.5,10,7.5,7.5,7.5,10),3)
D <- c(0.75, 0.65, -0.35)
mvpd::dmvss(X-D, alpha=1.71, Q=Q)
#> 0.001940025 with absolute error < 6e-05
mvpd::dmvss(X  , alpha=1.71, Q=Q, delta=D)
#> 0.001940025 with absolute error < 6e-05

```
