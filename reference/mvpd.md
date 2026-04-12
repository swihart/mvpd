# Multivariate Product Distributions

The purpose of this package is to offer density, probability, and random
variate generating (abbreviated as \[d/p/r\], respectively) functions
for multivariate distributions that can be represented as a product
distribution. Specifically, the package will primarily focus on the
product of a multivariate normal distribution and a univariate random
variable. These product distributions are called Scale Mixtures of
Multivariate Normal Distributions, and for particular choices of the
univariate random variable distribution the resultant product
distribution may be a family of interest. For instance, the square-root
of a positive stable random variable multiplied by a multivariate normal
distribution is the multivariate subgaussian stable distribution.
Product distribution theory is applied for implementing their
computation.

## Multivariate subgaussian stable distributions

[`dmvss`](https://swihart.github.io/mvpd/reference/dmvss.md) –
multivariate subgaussian stable distribution density

[`pmvss`](https://swihart.github.io/mvpd/reference/pmvss.md) –
multivariate subgaussian stable distribution probabilities

[`rmvss`](https://swihart.github.io/mvpd/reference/rmvss.md) –
multivariate subgaussian stable distribution random variates

[`pmvss_mc`](https://swihart.github.io/mvpd/reference/pmvss_mc.md) –
Monte Carlo version of probabilities, using `rmvss`

[`fit_mvss`](https://swihart.github.io/mvpd/reference/fit_mvss.md) – Fit
a multivariate subgaussian stable distribution (e.g. estimate parameters
given data)

## See also

Useful links:

- <https://github.com/swihart/mvpd>

- <https://swihart.github.io/mvpd/>

- Report bugs at <https://github.com/swihart/mvpd/issues>

## Author

**Maintainer**: Bruce Swihart <bruce.swihart@gmail.com>
([ORCID](https://orcid.org/0000-0002-4216-9942))
