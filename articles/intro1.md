# Introduction and Primer II

![](intro1_files/figure-html/unnamed-chunk-4-1.png)

- U is a `n x 2` matrix containing n random draws from the uniform
  circle.
- UA is the result of the matrix multiplication between `n x 2` U and
  `2 x 2` A, the Cholesky decomposition of a `2x2` matrix Q.
  - A makes a circle (U) into an ellipse (UA)
  - A can be thought of as a “square root” of a matrix. A’A = Q.
- When R $\sim \chi$(df=2) distribution, RUA is the bivariate normal
  distribution G(0,Q), Equivalently
  - `mvpd` uses G for Gaussian
