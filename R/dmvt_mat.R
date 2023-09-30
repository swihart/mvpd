#' Multivariate t-Distribution Density for matrix inputs
#' 
#' 
#' Computes the the density function of the multivariate subgaussian stable
#' distribution for arbitrary degrees of freedom, shape matrices, and location vectors.
#' See Swihart and Nolan (2022).
#' 
#' @param x nxd matrix of n variates of d-dimension
#' @param df default to 1 (Cauchy). This is df for t-dist, real number df>0. Can be non-integer.
#' @param delta location vector
#' @param Q Shape matrix.  See Nolan (2013).
#' @param outermost.int select which integration function to use for outermost
#' integral.  Default is "stats::integrate" and one can specify the following options 
#' with the `.si` suffix.  For diagonal Q, one can also specify "cubature::adaptIntegrate"
#' and use the `.ai` suffix options below (currently there is a bug for non-diagnoal Q).
#' @param spherical default is FALSE.  If true, use the spherical transformation.  
#' Results will be identical to spherical = FALSE but may be faster.
#' @param subdivisions.si the maximum number of subintervals.
#' The suffix \code{.si} indicates a \code{stats::integrate()} 
#' option for the outermost semi-infinite integral in the product distribution formulation.
#' @param rel.tol.si relative accuracy requested.  
#' The suffix \code{.si} indicates a \code{stats::integrate()} 
#' option for the outermost semi-infinite integral in the product distribution formulation.
#' @param abs.tol.si absolute accuracy requested. The suffix \code{.si} indicates a \code{stats::integrate()} 
#' option for the outermost semi-infinite integral in the product distribution formulation.
#' @param stop.on.error.si logical. If true (the default) an error stops the function. 
#' If false some errors will give a result with a warning in the message component.
#' The suffix \code{.si} indicates a \code{stats::integrate()} 
#' option for the outermost semi-infinite integral in the product distribution formulation.
#' @param tol.ai The maximum tolerance, default 1e-5.
#' The suffix \code{.ai} indicates a \code{cubature::adaptIntegrate} type 
#' option for the outermost semi-infinite integral in the product distribution formulation.
#' @param fDim.ai The dimension of the integrand, default 1, bears no
#' relation to the dimension of the hypercube
#' The suffix \code{.ai} indicates a \code{cubature::adaptIntegrate} type 
#' option for the outermost semi-infinite integral in the product distribution formulation.
#' @param maxEval.ai The maximum number of function evaluations needed,
#' default 0 implying no limit
#' The suffix \code{.ai} indicates a \code{cubature::adaptIntegrate} type 
#' option for the outermost semi-infinite integral in the product distribution formulation.
#' @param absError.ai The maximum absolute error tolerated
#' The suffix \code{.ai} indicates a \code{cubature::adaptIntegrate} type 
#' option for the outermost semi-infinite integral in the product distribution formulation.
#' @param doChecking.ai A flag to be thorough checking inputs to
#' C routines. A FALSE value results in approximately 9 percent speed
#' gain in our experiments. Your mileage will of course vary. Default
#' value is FALSE.
#' The suffix \code{.ai} indicates a \code{cubature::adaptIntegrate} type 
#' option for the outermost semi-infinite integral in the product distribution formulation.
#' @param which.stable defaults to "libstable4u", other option is "stabledist".  Indicates which package 
#' should provide the univariate stable distribution in this production distribution form of a univariate
#' stable and multivariate normal.
#' @return The object returned depends on what is selected for \code{outermost.int}.  In the case of the default, 
#' \code{stats::integrate}, the value is a list of class "integrate" with components:
#' \itemize{
#' \item{\code{value}}{ the final estimate of the integral.}
#' \item{\code{abs.error}}{ estimate of the modulus of the absolute error.}
#' \item{\code{subdivisions}}{ the number of subintervals produced in the subdivision process.}
#' \item{\code{message}}{ "OK" or a character string giving the error message.}
#' \item{\code{call}}{ the matched call.}
#' }
#' Note: The reported \code{abs.error} is likely an under-estimate as \code{integrate}
#' assumes the integrand was without error, 
#' which is not the case in this application.  
#' @references
#' 
#' Swihart & Nolan, "The R Journal: Multivariate Subgaussian Stable Distributions in R", The R Journal, 2022
#' 
#' @keywords distribution
#' @importFrom cubature adaptIntegrate
#' @importFrom matrixStats rowProds
#' @importFrom stabledist dstable
#' @importFrom libstable4u stable_pdf
#' @importFrom mvtnorm pmvnorm GenzBretz
#' @importFrom stats integrate dnorm
#' @examples
#' 
#' x <- c(1.23, 4.56)
#' mu <- 1:2
#' Sigma <- matrix(c(4, 2, 2, 3), ncol=2)
#' df01 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  1, log=FALSE) # default log = TRUE!
#' df10 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df = 10, log=FALSE) # default log = TRUE!
#' df30 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df = 30, log=FALSE) # default log = TRUE!
#' df01
#' df10
#' df30
#' 
#' 
#' dmvt_mat(
#'   matrix(x,ncol=2),
#'   df = 1,
#'   Q = Sigma,
#'   delta=mu)$int
#' 
#' 
#' dmvt_mat(
#'   matrix(x,ncol=2),
#'   df = 10,
#'   Q = Sigma,
#'   delta=mu)$int
#' 
#' 
#' dmvt_mat(
#'   matrix(x,ncol=2),
#'   df = 30,
#'   Q = Sigma,
#'   delta=mu)$int
#' 
#' ## Q: can we do non-integer degrees of freedom?
#' ## A: yes for both mvpd::dmvt_mat and mvtnorm::dmvt
#' 
#' df1.5 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  1.5, log=FALSE) # default log = TRUE!
#' df1.5
#' 
#' dmvt_mat(
#'   matrix(x,ncol=2),
#'   df = 1.5,
#'   Q = Sigma,
#'   delta=mu)$int
#' 
#' 
#' ## Q: can we do <1 degrees of freedom but >0?
#' ## A: yes for both mvpd::dmvt_mat and mvtnorm::dmvt
#' 
#' df0.5 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  0.5, log=FALSE) # default log = TRUE!
#' df0.5
#' 
#' dmvt_mat(
#'   matrix(x,ncol=2),
#'   df = 0.5,
#'   Q = Sigma,
#'   delta=mu)$int
#' 
#' df0.0001 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  0.0001, log=FALSE) # default log = TRUE!
#' df0.0001
#' 
#' dmvt_mat(
#'   matrix(x,ncol=2),
#'   df = 0.0001,
#'   Q = Sigma,
#'   delta=mu)$int
#' 
#' 
#' 
#' ## Q: can we do ==0 degrees of freedom?
#' ## A: No for both mvpd::dmvt_mat and mvtnorm::dmvt
#' 
#' ## this just becomes normal, as per the manual for mvtnorm::dmvt
#' df0.0 <- mvtnorm::dmvt(x, delta = mu, sigma = Sigma, df =  0, log=FALSE) # default log = TRUE!
#' df0.0
#' 
#' dmvt_mat(
#'   matrix(x,ncol=2),
#'   df = 0,
#'   Q = Sigma,
#'   delta=mu)$int 
#' 
#' @export
dmvt_mat <- function(x, df=1, Q = NULL, delta=rep(0,d), 
                          
                      outermost.int = c("stats::integrate","cubature::adaptIntegrate","cubature::hcubature")[2],
                      
                      spherical = FALSE,
                      
                      subdivisions.si = 100L,
                      rel.tol.si = .Machine$double.eps^0.25, 
                      abs.tol.si = rel.tol.si,
                      stop.on.error.si = TRUE,
                      
                      tol.ai = 1e-05,
                      fDim.ai = NULL,
                      maxEval.ai = 0,
                      absError.ai=0,
                      doChecking.ai=FALSE,
                      
                      which.stable=c("libstable4u", "stabledist")[1]
                      
){
  alpha <- df ## legacy.
  f_A <- switch(which.stable,
                "libstable4u" =
                    function(x, alpha){ 
                      nu <- alpha
                      a.ig <- b.ig <- nu/2
                      
                      b.ig^a.ig / gamma(a.ig) * x^{-a.ig-1} * exp(-b.ig/x)
                      
                    },
                "stabledist"=
                  function(x, alpha) 
                    stabledist::dstable(x,
                                        alpha = alpha/2,
                                        beta = 1,
                                        gamma = 2*cos(pi*alpha/4)^(2/alpha),
                                        delta = 0,
                                        pm = 1)
  )
  
  
  
  
  f_B <- switch(which.stable,
                "libstable4u" = 
                  function(x, alpha){ 
                    nu = alpha
                    2*x*f_A(x^2, nu)
                  },
                "stabledist" = 
                  function(x, alpha)
                    2*x*
                  stabledist::dstable(x^2,
                                      alpha = alpha/2,
                                      beta = 1,
                                      gamma = 2*cos(pi*alpha/4)^(2/alpha),
                                      delta = 0,
                                      pm = 1)
  )  
  d <- NCOL(x) ## length(x)
  x <- t(apply(x, 1, function(W) W - delta)) ##x - delta
  ifelse(!spherical,
         dens <- function(b,x,Qmat=Q,alp=alpha) f_B(b, alp) * 1/abs(b)^d * mvtnorm::dmvnorm(x/b, sigma=Qmat),
         dens <- function(b,x,Qmat=Q,alp=alpha){
           Linv <- solve(t(chol(Qmat)))
           detLinv <- det(Linv)
           f_B(b, alpha) * 1/abs(b)^d *detLinv *prod(dnorm(t(Linv %*% matrix(x/b,ncol=1))))
         }
  )
  
  switch(outermost.int,
         "cubature::adaptIntegrate"=
           adaptIntegrate_inf_limPD(dens,
                                    lowerLimit=0,
                                    upperLimit=Inf,
                                    x=x,
                                    Q=Q,
                                    tol.ai = tol.ai,
                                    fDim.ai = NROW(x),
                                    maxEval.ai = maxEval.ai,
                                    absError.ai=absError.ai,
                                    doChecking.ai=doChecking.ai
           )
         
         ,
         "cubature::hcubature"=
           cubature::hcubature(dens,
                               lowerLimit=0,
                               upperLimit=Inf,
                               x=x,
                               Q=Q,
                               tol = tol.ai,
                               fDim = NROW(x),
                               maxEval = maxEval.ai,
                               absError=absError.ai,
                               doChecking=doChecking.ai
           )
         
         ,
         "stats::integrate" = 
           apply(matrix(1:NROW(x), nrow=NROW(x)), 
                 1, 
                 function(W){ 
                   integrate(Vectorize(dens,vectorize.args = "b"),
                             lower=0,
                             upper=Inf,
                             x=x[W,],
                             Q=Q,
                             subdivisions = subdivisions.si,
                             rel.tol=rel.tol.si,
                             abs.tol=abs.tol.si,
                             stop.on.error=stop.on.error.si)}
           )
  )
}