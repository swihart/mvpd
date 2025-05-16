#' Density for the Kolmogorov Distribution
#'
#' @param x domain value.
#' @param nterms the number of terms in the limiting form's sum. 
#' That is, changing the infinity on the top of the summation to a big K.
#' @param rep the representation.  See article on webpage. Default is 'K3'.
#' @param K3cutpt the cutpoint for rep='K3'. Seee article on webpage.
#'
#' @returns the value of the density at specified x
#' @export
#'
#' @examples
#' ## see https://swihart.github.io/mvpd/articles/deep_dive_kolm.html
#' dkolm(1)
dkolm <- Vectorize(function(x, nterms=500, rep="K3", K3cutpt=2){
  
  k <- 1:nterms
  
  switch(rep,
         K1 =  -2*sum( (-1)^(k-1) * -4*x*k^2 * exp(-2*k^2*x^2) ),
         K2 = {
           const <- sqrt(pi/2)
           denom <- 2*x^4
           numer <- sum(
             exp(-(1-2*k)^2 * pi^2 / (8*x^2)) *
               ((1-2*k)^2*pi^2-4*x^2)
           )
           const * numer / denom
         },
         K3 = {
           ifelse(x < K3cutpt,
                  {
                    const <- sqrt(pi/2)
                    denom <- 2*x^4
                    numer <- sum(
                      exp(-(1-2*k)^2 * pi^2 / (8*x^2)) *
                        ((1-2*k)^2*pi^2-4*x^2)
                    )
                    const * numer / denom  
                  },
                  -2*sum( (-1)^(k-1) * -4*x*k^2 * exp(-2*k^2*x^2) ))
         }
  )
}, "x"
)