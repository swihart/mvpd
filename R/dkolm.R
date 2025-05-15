
#' Title
#'
#' @param x 
#' @param nterms 
#' @param rep 
#' @param K3cutpt 
#'
#' @returns
#' @export
#'
#' @examples
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