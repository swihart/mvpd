#' Multivariate Logistic Random Variables
#'
#'
#' Computes random vectors of the multivariate symmetric logistic
#' distribution for arbitrary correlation matrices using the asymptotic
#' Kolmogorov distribution -- see references.
#'
#'
#' @param n number of observations
#' @param delta location vector.
#' @param Q semi-positive definite
#' @param BIG the number of exponential to add for asymptotic Kolomogrov
#' 
#' @references
#'
#' Scale Mixtures of Normal Distributions
#' Author(s): D. F. Andrews and C. L. Mallows
#' Source: Journal of the Royal Statistical Society. Series B (Methodological), Vol. 36, No. 1
#' (1974), pp. 99-102
#' Published by: Wiley for the Royal Statistical Society
#' Stable URL: http://www.jstor.org/stable/2984774
#'
#' @keywords distribution
#' @importFrom stats rnorm
#' @importFrom stats rexp
#' @examples
#' rmvlogis(10, Q=diag(5))
#'
#' \dontrun{
#' QMAT <- matrix(c(1,0,0,1),nrow=2)
#' draw_NNMD  <- NonNorMvtDist::rmvlogis(2e3, parm1=rep(0,2), parm2=rep(1,2))
#' draw_mvpd  <-          mvpd::rmvlogis(2e3,     Q=QMAT)
#'
#' mean(draw_NNMD[,1]   < -1 & draw_NNMD[,2]   < 3)
#' mean(draw_mvpd[,1] < -1 & draw_mvpd[,2] < 3)
#' 
#' plogis(-1)
#' mean(draw_NNMD[,1] < -1)
#' mean(draw_mvpd[,1] < -1)
#'
#' plogis(3)
#' mean(draw_NNMD[,2] < 3)
#' mean(draw_mvpd[,2] < 3)
#'  
#' rangex <- range(c(draw_mvpd[,1],draw_NNMD[,1]))
#' rangey <- range(c(draw_mvpd[,2],draw_NNMD[,2]))
#'
#' par(mfrow=c(3,2), pty="s", mai=c(.5,.1,.1,.1))
#' plot(draw_NNMD, xlim=rangex, ylim=rangey); abline(h=0,v=0)
#' plot(draw_mvpd   , xlim=rangex, ylim=rangey); abline(h=0,v=0)
#'
#' hist(draw_NNMD[,1]  , breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
#' curve(dlogis(x), add=TRUE, col="blue",lwd=2)
#' hist(draw_mvpd[,1], breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
#' curve(dlogis(x), add=TRUE, col="blue",lwd=2)
#' hist(draw_NNMD[,2]  , breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
#' curve(dlogis(x), add=TRUE, col="blue",lwd=2)
#' hist(draw_mvpd[,2], breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
#' curve(dlogis(x), add=TRUE, col="blue",lwd=2)
#' }
#' @export
rmvlogis <- function(n, Q = NULL, delta=rep(0,d), BIG=500){
  # d <- nrow(Q)
  # 
  # A <- matrix(stats::rexp(n*BIG), n, BIG) %*% matrix(1/c(1:BIG)^2,nrow=BIG)
  # 
  # G <-   matrix(rnorm(n * d), n, d) %*% chol(Q)
  # 
  # c(sqrt(2*A)) * G + matrix(delta,nrow=n,ncol=length(delta),byrow=T)
  
  d <- nrow(Q)
  
  V2 <- 2*matrix(stats::rexp(n*BIG), n, BIG) %*% matrix(1/c(1:BIG)^2,nrow=BIG)
  
  G <-   matrix(rnorm(n * d), n, d) %*% chol(Q)
  
  c(sqrt(V2)) * G + matrix(delta,nrow=n,ncol=length(delta),byrow=T)
  
}
