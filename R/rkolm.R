#' Random Variates for the Kolmogorov Distribution 
#'
#' @param n the number of random variate to simulate
#' @param nterms the number of terms in the limiting sum. That is, turning
#' infinity into a Big K on the top of the summation.
#'
#' @returns n random variates
#' @export
#'
#' @examples
#' ## see https://swihart.github.io/mvpd/articles/deep_dive_kolm.html
#' rkolm(10)
rkolm <- function(n, nterms=500){
  #k <- 1:nterms

  ## this is from L(x/2)
# L_x_2 <- 2*matrix(stats::rexp(n*nterms, rate=1), n, nterms) %*%
#   matrix(1/c(1:nterms)^2,nrow=nterms)
  ## this is from L(x)
  L_x <-     matrix(stats::rexp(n*nterms, rate=2), n, nterms) %*%
    matrix(1/c(1:nterms)^2,nrow=nterms)
  sqrt(L_x)

  
  ## this is NOT faster than matrix approach
  # holder <- rep(NA,n)
  # ## faster than matrix?
  # denoms <- 1/c(1:nterms)^2
  # 
  # for(i in 1:n){
  # numers <- stats::rexp(nterms, rate=2)
  # 
  # holder[i] <- sqrt(sum(numers * denoms))
  # }
  # holder
  
  }