# Density for the Kolmogorov Distribution

Density for the Kolmogorov Distribution

## Usage

``` r
dkolm(x, nterms = 500, rep = "K3", K3cutpt = 2)
```

## Arguments

- x:

  domain value.

- nterms:

  the number of terms in the limiting form's sum. That is, changing the
  infinity on the top of the summation to a big K.

- rep:

  the representation. See article on webpage. Default is 'K3'.

- K3cutpt:

  the cutpoint for rep='K3'. Seee article on webpage.

## Value

the value of the density at specified x

## Examples

``` r
## see https://swihart.github.io/mvpd/articles/deep_dive_kolm.html
dkolm(1)
#> [1] 1.071949
```
