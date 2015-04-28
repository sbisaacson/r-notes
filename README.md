% R notes
% Samuel Isaacson
% April 28, 2015


```r
suppressPackageStartupMessages({
    library("knitr")
    library("assertthat")
    library("printr")
    library("data.table")
})
```

# Array permutations

I can never remember which way array permutations act.


```
## [1] TRUE
```

```
## [1] TRUE
```

```
## [1] TRUE
```

```
## [1] TRUE
```

# Random number generation

Use this to evaluate an expression while temporarily reseeding the
RNG:


```
## [1] TRUE
```

# Histograms in data.table

I almost always use
[`dplyr`](http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html)
instead of
[`data.table`](http://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.pdf).
This is one task that's impossible with `dplyr`:


```r
x <- data.table(z = rnorm(1000), key = "z")
x_breaks <- data.table(z = seq(min(x$z), max(x$z), length.out = 30), key = "z")
x_breaks[x, .(count = .N), roll = TRUE, by = z]
```



|          z| count|
|----------:|-----:|
| -3.3617135|     1|
| -3.1537210|     1|
| -2.9457284|     1|
| -2.7377358|     4|
| -2.5297432|     3|
| -2.3217506|     6|
| -2.1137580|    11|
| -1.9057654|    22|
| -1.6977728|    22|
| -1.4897802|    41|
| -1.2817876|    37|
| -1.0737950|    40|
| -0.8658024|    63|
| -0.6578098|    70|
| -0.4498172|    68|
| -0.2418246|    93|
| -0.0338321|    80|
|  0.1741605|    78|
|  0.3821531|    86|
|  0.5901457|    72|
|  0.7981383|    53|
|  1.0061309|    37|
|  1.2141235|    35|
|  1.4221161|    25|
|  1.6301087|    21|
|  1.8381013|    17|
|  2.0460939|     9|
|  2.4620791|     3|
|  2.6700717|     1|


# Useful miscellaneous utilities

When I miss MATLAB:


```r
ones <- function (...) array(1, dim = c(...))
```

This is convenient when combined with `%o%`, but indexing a vector by
an array drops dimensions, so it isn't as flexible.

Here is a useful utility for viewing a huge amount of data in the
console:


```r
ppage <- function (x) page(x, method = "print", max = 99999L)
```
