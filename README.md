# R notes

## Introduction

This document is just a grab-bag of notes and gripes for R.


```r
suppressPackageStartupMessages({
    library("knitr")
    library("assertthat")
    library("printr")
    library("data.table")
    library("dplyr")
    library("magrittr")
    library("tidyr")
})

set.seed(11235)
```

## Pitfalls

### Ambiguous border cases

It is a commonplace that you should use `seq_len(n)` instead of `1:n`
because of the behavior when `n == 0`:


```r
1:0
```

```
## [1] 1 0
```

```r
seq_len(0)
```

```
## integer(0)
```

These pitfalls are everywhere. R has a bewildering number of names
used at startup:


```r
## NB. get is not vectorized.

data_frame(package = paste0("package:",
               c("base", "methods", "datasets", "utils",
                 "grDevices", "graphics", "stats")),
           name = lapply(package, ls)) %>%
    unnest(name) %>%
    filter(vapply(seq_len(nrow(.)),
               function (r) is.function(get(.$name[r], .$package[r])),
               logical(1))) %>%
    group_by(package) %>%
    tally %>%
    arrange(desc(n))
```



|package           |    n|
|:-----------------|----:|
|package:base      | 1193|
|package:stats     |  445|
|package:methods   |  216|
|package:utils     |  203|
|package:grDevices |  104|
|package:graphics  |   87|

Many of these methods try to guess your intention based on the
length or dimensions of the argument, which can lead to odd bugs.
Compare the following invocations of `diag`:


```r
list(diag(3), diag(3, 4), diag(c(3, 4)))
```

```
## [[1]]
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1
## 
## [[2]]
##      [,1] [,2] [,3] [,4]
## [1,]    3    0    0    0
## [2,]    0    3    0    0
## [3,]    0    0    3    0
## [4,]    0    0    0    3
## 
## [[3]]
##      [,1] [,2]
## [1,]    3    0
## [2,]    0    4
```

### Encoding

Whenever reading or writing data, make sure to specify the encoding
(so pass `encoding = "latin1"` or `encoding = "UTF-8"` to `read.csv`
or `file`). Your code may work fine on Windows but fail on UNIX, or
*vice versa*.

### unwrap-protect

Use `on.exit` to avoid resource leaks.

### Missing functions

Even though R has thousands of functions, it is missing `hypot` (which
is
[not trivial to implement](http://www.johndcook.com/blog/2010/06/02/whats-so-hard-about-finding-a-hypotenuse/))
and a few other numerical niceties.

## Vectorized functions

Always use `rowSums`, `colSums`, `rowMeans`, and `colMeans` instead of
the equivalent `apply` call.

## Array permutations

I can never remember which way array permutations act.


```r
test_array <- array(seq_len(48), dim = c(2, 2, 4, 3))
test_array_perm <- aperm(test_array, perm = c(2, 3, 4, 1))

assert_that(all(dim(test_array_perm) == c(2, 4, 3, 2)))

## Inline matrix version of expand.grid:

`%g%` <- function (m1, m2) {
    if (is.null(dim(m1))) dim(m1) <- c(length(m1), 1)
    if (is.null(dim(m2))) dim(m2) <- c(length(m2), 1)
    n1 <- nrow(m1)
    n2 <- nrow(m2)
    cbind(m1[rep(seq_len(n1), n2), ],
          m2[rep(seq_len(n2), each = n1), ])
}

test_indices <- 1:2 %g% 1:2 %g% 1:4 %g% 1:3

assert_that(all(test_array[test_indices] == seq_len(48)))
assert_that(test_array[1, 2, 4, 3] == test_array_perm[2, 4, 3, 1])

## More generally:

assert_that(all(test_array[test_indices] ==
                test_array_perm[test_indices[, c(2, 3, 4, 1)]]))
```

## Random number generation

Use this to evaluate an expression while temporarily reseeding the
RNG:


```r
with_seed <- function (seed, expr) {
    save_seed <-
        if (exists(".Random.seed", .GlobalEnv)) .Random.seed else NULL
    on.exit(
        if (!is.null(save_seed))
            assign(".Random.seed", save_seed, envir = .GlobalEnv),
        add = TRUE)
    set.seed(seed)
    eval(expr, envir = parent.frame())
}

assert_that(
    all(with_seed(1000, rnorm(9)[c(1:6, 1:3, 7:9)]) ==
            with_seed(1000, c(rnorm(6), with_seed(1000, rnorm(3)), rnorm(3)))))
```

## Histograms in data.table

I almost always use
[`dplyr`](http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html)
instead of
[`data.table`](http://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.pdf).
This is one task that's impossible with `dplyr`:


```r
local({
    x <- data.table(z = rnorm(1000), key = "z")
    x_breaks <- data.table(z = seq(min(x$z), max(x$z), length.out = 10), key = "z")
    x_breaks[x, .(count = .N), roll = TRUE, by = z]
})
```



|          z| count|
|----------:|-----:|
| -3.0055422|     8|
| -2.3214054|    45|
| -1.6372685|   121|
| -0.9531317|   219|
| -0.2689949|   261|
|  0.4151420|   198|
|  1.0992788|   108|
|  1.7834156|    30|
|  2.4675524|     9|
|  3.1516893|     1|

## Linear algebra

For some reason, R has `chol2inv` but not `chol2solve`.


```r
chol2solve <- function (R, b) {
    assert_that(is.vector(b) || (is.matrix(b) && nrow(b) == nrow(R)))
    backsolve(R, backsolve(R, b, transpose = TRUE))
}

assert_that(all.equal(solve(matrix(c(1, 2, 2, 5), 2, 2), c(1, 1)),
                      chol2solve(chol(matrix(c(1, 2, 2, 5), 2, 2)), c(1, 1))))
```

## Useful miscellaneous utilities

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
