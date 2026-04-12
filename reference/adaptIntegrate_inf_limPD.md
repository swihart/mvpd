# Adaptive multivariate integration over hypercubes (admitting infinite limits)

The function performs adaptive multidimensional integration (cubature)
of (possibly) vector-valued integrands over hypercubes. It is a wrapper
for cubature:::adaptIntegrate, transforming (-)Inf appropriately as
described in cubature's help page
(http://ab-initio.mit.edu/wiki/index.php/Cubature#Infinite_intervals).

## Usage

``` r
adaptIntegrate_inf_limPD(
  f,
  lowerLimit,
  upperLimit,
  ...,
  tol.ai = 1e-05,
  fDim.ai = 1,
  maxEval.ai = 0,
  absError.ai = 0,
  doChecking.ai = FALSE
)
```

## Arguments

- f:

  The function (integrand) to be integrated

- lowerLimit:

  The lower limit of integration, a vector for hypercubes

- upperLimit:

  The upper limit of integration, a vector for hypercubes

- ...:

  All other arguments passed to the function f

- tol.ai:

  The maximum tolerance, default 1e-5.

- fDim.ai:

  The dimension of the integrand, default 1, bears no relation to the
  dimension of the hypercube

- maxEval.ai:

  The maximum number of function evaluations needed, default 0 implying
  no limit

- absError.ai:

  The maximum absolute error tolerated

- doChecking.ai:

  A flag to be thorough checking inputs to C routines. A FALSE value
  results in approximately 9 percent speed gain in our experiments. Your
  mileage will of course vary. Default value is FALSE.

## Examples

``` r
## integrate Cauchy Density from -Inf to Inf
adaptIntegrate_inf_limPD(function(x) 1/pi * 1/(1+x^2), -Inf, Inf)
#> $integral
#> [1] 1
#> 
#> $error
#> [1] 1.901365e-06
#> 
#> $functionEvaluations
#> [1] 45
#> 
#> $returnCode
#> [1] 0
#> 
adaptIntegrate_inf_limPD(function(x, scale) 1/(pi*scale) * 1/(1+(x/scale)^2), -Inf, Inf, scale=4)
#> $integral
#> [1] 1
#> 
#> $error
#> [1] 2.705388e-06
#> 
#> $functionEvaluations
#> [1] 165
#> 
#> $returnCode
#> [1] 0
#> 
## integrate Cauchy Density from -Inf to -3
adaptIntegrate_inf_limPD(function(x) 1/pi * 1/(1+x^2), -Inf, -3)$int
#> [1] 0.1024164
stats::pcauchy(-3)
#> [1] 0.1024164
adaptIntegrate_inf_limPD(function(x, scale) 1/(pi*scale) * 1/(1+(x/scale)^2), -Inf, -3, scale=4)$int
#> [1] 0.2951672
stats::pcauchy(-3, scale=4)
#> [1] 0.2951672
```
