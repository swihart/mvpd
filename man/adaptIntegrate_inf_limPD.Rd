% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adaptIntegrate_inf_lim.R
\name{adaptIntegrate_inf_limPD}
\alias{adaptIntegrate_inf_limPD}
\title{Adaptive multivariate integration over hypercubes (admitting infinite limits)}
\usage{
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
}
\arguments{
\item{f}{The function (integrand) to be integrated}

\item{lowerLimit}{The lower limit of integration, a vector for
hypercubes}

\item{upperLimit}{The upper limit of integration, a vector for
hypercubes}

\item{...}{All other arguments passed to the function f}

\item{tol.ai}{The maximum tolerance, default 1e-5.}

\item{fDim.ai}{The dimension of the integrand, default 1, bears no
relation to the dimension of the hypercube}

\item{maxEval.ai}{The maximum number of function evaluations needed,
default 0 implying no limit}

\item{absError.ai}{The maximum absolute error tolerated}

\item{doChecking.ai}{A flag to be thorough checking inputs to
C routines. A FALSE value results in approximately 9 percent speed
gain in our experiments. Your mileage will of course vary. Default
value is FALSE.}
}
\description{
The function performs adaptive multidimensional integration
(cubature) of (possibly) vector-valued integrands over hypercubes.
It is a wrapper for cubature:::adaptIntegrate, transforming (-)Inf
appropriately as described in cubature's help page (http://ab-initio.mit.edu/wiki/index.php/Cubature#Infinite_intervals).
}
\examples{
## integrate Cauchy Density from -Inf to Inf
adaptIntegrate_inf_limPD(function(x) 1/pi * 1/(1+x^2), -Inf, Inf)
adaptIntegrate_inf_limPD(function(x, scale) 1/(pi*scale) * 1/(1+(x/scale)^2), -Inf, Inf, scale=4)
## integrate Cauchy Density from -Inf to -3
adaptIntegrate_inf_limPD(function(x) 1/pi * 1/(1+x^2), -Inf, -3)$int
stats::pcauchy(-3)
adaptIntegrate_inf_limPD(function(x, scale) 1/(pi*scale) * 1/(1+(x/scale)^2), -Inf, -3, scale=4)$int
stats::pcauchy(-3, scale=4)

}
\keyword{integration}
\keyword{multivariate}
\keyword{numerical}
