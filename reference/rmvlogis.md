# Multivariate Logistic Random Variables

Computes random vectors of the multivariate symmetric logistic
distribution for arbitrary correlation matrices using the asymptotic
Kolmogorov distribution – see references.

## Usage

``` r
rmvlogis(n, Q = NULL, delta = rep(0, d), BIG = 500)
```

## Arguments

- n:

  number of observations

- Q:

  semi-positive definite

- delta:

  location vector.

- BIG:

  the number of exponential to add for asymptotic Kolomogrov

## References

Scale Mixtures of Normal Distributions Author(s): D. F. Andrews and C.
L. Mallows Source: Journal of the Royal Statistical Society. Series B
(Methodological), Vol. 36, No. 1 (1974), pp. 99-102 Published by: Wiley
for the Royal Statistical Society Stable URL:
http://www.jstor.org/stable/2984774

## Examples

``` r
rmvlogis(10, Q=diag(5))
#>             [,1]       [,2]       [,3]       [,4]        [,5]
#>  [1,]  1.1770287  1.5226579  0.6974276  0.8467555  0.09429795
#>  [2,] -0.1636597  2.4184144 -3.2223124 -1.5816272 -3.98359028
#>  [3,]  2.2395249 -4.2123205 -2.3444725 -0.7518423 -0.25102021
#>  [4,] -1.8916177  2.9053885  1.4103430 -1.0662243  0.75352411
#>  [5,]  0.4784924 -0.3034091 -0.0250878  0.8599325 -0.10400652
#>  [6,]  2.1733984  1.0380266 -0.2616401  0.4669456 -0.69642319
#>  [7,] -0.8854321  0.8550146 -3.0059695 -0.1536785 -1.60043221
#>  [8,]  2.2554228 -0.9378274 -2.1335241 -3.3860767  1.12269757
#>  [9,]  1.0174335 -1.2826962  0.2050895  1.1857028  0.16163594
#> [10,]  2.6151071  0.7222037  1.2803756  0.6625827 -0.59042839

if (FALSE) { # \dontrun{
QMAT <- matrix(c(1,0,0,1),nrow=2)
draw_NNMD  <- NonNorMvtDist::rmvlogis(2e3, parm1=rep(0,2), parm2=rep(1,2))
draw_mvpd  <-          mvpd::rmvlogis(2e3,     Q=QMAT)

mean(draw_NNMD[,1]   < -1 & draw_NNMD[,2]   < 3)
mean(draw_mvpd[,1] < -1 & draw_mvpd[,2] < 3)

plogis(-1)
mean(draw_NNMD[,1] < -1)
mean(draw_mvpd[,1] < -1)

plogis(3)
mean(draw_NNMD[,2] < 3)
mean(draw_mvpd[,2] < 3)
 
rangex <- range(c(draw_mvpd[,1],draw_NNMD[,1]))
rangey <- range(c(draw_mvpd[,2],draw_NNMD[,2]))

par(mfrow=c(3,2), pty="s", mai=c(.5,.1,.1,.1))
plot(draw_NNMD, xlim=rangex, ylim=rangey); abline(h=0,v=0)
plot(draw_mvpd   , xlim=rangex, ylim=rangey); abline(h=0,v=0)

hist(draw_NNMD[,1]  , breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
curve(dlogis(x), add=TRUE, col="blue",lwd=2)
hist(draw_mvpd[,1], breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
curve(dlogis(x), add=TRUE, col="blue",lwd=2)
hist(draw_NNMD[,2]  , breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
curve(dlogis(x), add=TRUE, col="blue",lwd=2)
hist(draw_mvpd[,2], breaks = 100,  xlim=rangex, probability=TRUE, ylim=c(0,.40))
curve(dlogis(x), add=TRUE, col="blue",lwd=2)
} # }
```
