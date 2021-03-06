# R notes

## Introduction

This document is just a grab-bag of notes and gripes for R.


```r
suppressPackageStartupMessages({
    library("Matrix")
    library("knitr")
    library("printr")
    library("data.table")
    library("dplyr")       # masks data.table::{between,last}, etc.
    library("magrittr")
    library("tidyr")       # masks magrittr::extract, Matrix::expand
    library("assertthat")
    library("testthat")    # masks magrittr::{equals,is_less_than,not}
})

set.seed(11235)
```

## Pitfalls

### R is huge

R has a bewildering number of functions already reserved at startup:


```r
## NB. get is not vectorized.

data_frame(package = paste0("package:",
               c("base", "methods", "datasets", "utils",
                 "grDevices", "graphics", "stats", "Matrix")),
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
|package:Matrix    |  116|
|package:grDevices |  104|
|package:graphics  |   87|

If you want to do something, it is probably in `base` somewhere. "A
month in the lab can save an hour in the library." Try `help(package =
base)` before writing your own code.

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


Many R functions try to guess your intention based on the length or
dimensions of the argument, which can lead to odd bugs. Compare the
following invocations of `diag`:


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

### Indexing

If `x` is a vector and you index `x` with a logical vector `v`, R will
extract the entries of `x` for which the corresponding entry of `v`
(recycled if necessary) is `TRUE`:


```r
logical_indexing <- rnorm(12)
logical_indexing
```

```
##  [1] -0.297055910 -1.423666458 -0.216315264 -0.005881971 -0.859138737
##  [6] -2.095621078 -0.265722558 -0.103193839 -0.799971279  0.514216402
## [11] -0.071860783  0.700654880
```

```r
logical_indexing[TRUE]
```

```
##  [1] -0.297055910 -1.423666458 -0.216315264 -0.005881971 -0.859138737
##  [6] -2.095621078 -0.265722558 -0.103193839 -0.799971279  0.514216402
## [11] -0.071860783  0.700654880
```

```r
logical_indexing[c(TRUE, FALSE, FALSE)]
```

```
## [1] -0.297055910 -0.005881971 -0.265722558  0.514216402
```

Be careful: if you accidentally convert these logical values to
integers, then R will use integer indexing:


```r
logical_indexing[1]
```

```
## [1] -0.2970559
```

```r
logical_indexing[c(1, 0, 0)]
```

```
## [1] -0.2970559
```

```r
all(c(1, 0, 0) == c(TRUE, FALSE, FALSE))
```

```
## [1] TRUE
```

Beware of indexing by `0` in general. You might object that the last
line is disingenuous, as we should be using `identical` or perhaps
`all.equal`:


```r
list(identical(c(1, 0, 0), c(TRUE, FALSE, FALSE)),
     all.equal(c(1, 0, 0), c(TRUE, FALSE, FALSE)))
```

```
## [[1]]
## [1] FALSE
## 
## [[2]]
## [1] "Modes: numeric, logical"              
## [2] "target is numeric, current is logical"
```

Fair enough. So `TRUE == 1`, but `TRUE` and `1` are not identical, nor
are they "nearly equal."

### Encoding

Whenever reading or writing data, make sure to specify the encoding
(so pass `encoding = "latin1"` or `encoding = "UTF-8"` to `read.csv`
or `file`). Your code may work fine on Windows but fail on UNIX, or
vice versa.

### unwind-protect

Use `on.exit` to avoid resource leaks.

### Missing functions

Even though R has thousands of functions, it is missing `hypot` (which
is
[not trivial to implement](http://www.johndcook.com/blog/2010/06/02/whats-so-hard-about-finding-a-hypotenuse/))
and a few other numerical niceties. As it happens, though, `hypot` is
available for matrix arguments with `norm(..., type = "F")`:


```r
c(norm(matrix(c(1e300, 2e300)), "F"), sqrt(sum(c(1e300, 2e300) ^ 2)))
```

```
## [1] 2.236068e+300           Inf
```

It a bit clumsy, but it avoids overflow.

### String stuff

Avoid using native R string functions in favor of `stringr` whenever
possible. Does this behavior make sense?


```r
data_frame(string = c("", "-", "/", "-/", "/-", "-/-"),
           `base pieces` =
               sapply(base::strsplit(string, "/"), length),
           `stringr pieces` =
               sapply(stringr::str_split(string, "/"), length)) %>%
    mutate(string = sprintf("\"%s\"", encodeString(string)))
```



|string | base pieces| stringr pieces|
|:------|-----------:|--------------:|
|""     |           0|              1|
|"-"    |           1|              1|
|"/"    |           1|              2|
|"-/"   |           1|              2|
|"/-"   |           2|              2|
|"-/-"  |           2|              2|

### Vectors and arrays

R really has two kinds of 1D vectors. An expression such as `1:5` has
no `dim` attribute, but you could set it if you want. I think having
no dim attribute for vanilla vectors is a strange design feature and
I've never really understood what the purpose of the distinction
between length and dim is supposed to be in the one-dimensional case.
R does not have scalars (which would be arrays with zero-length
dimension attributes). It will try to coerce a dimensionless vector to
what its designers envisioned as the most sensible interpretation, but
it depends on the context. Usually it just makes a column vector
(which, by the way, `matrix` with no `nrow` or `ncol` arguments will
do). But sometimes it makes a row vector: for instance, we have


```r
print(1:5 %*% 1:5)
```

```
##      [,1]
## [1,]   55
```

The first `1:5` becomes a row vector and the second a column vector.
But if we coerce the first argument to a column vector, suddenly the
second becomes a row:



```r
print(matrix(1:5) %*% 1:5)
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    2    3    4    5
## [2,]    2    4    6    8   10
## [3,]    3    6    9   12   15
## [4,]    4    8   12   16   20
## [5,]    5   10   15   20   25
```

The documentation says

    Since R 3.2.0, promotion of a vector to a 1-row or 1-column matrix
    happens in even more cases, when one of the two choices allows x
    and y to get conformable dimensions.

This is almost guaranteed to blow up if you write code that works with
matrices and you plug in something with only one row or one column,
since `drop = TRUE` is the default when indexing.

### Lists and NULL

I like to think of lists as arrays of boxed data. The `[` function
applied to a list acts like vector indexing and returns a list, while
`[[` only accepts a single index and unboxes the entry of the list.
You can't delete entries from numeric vectors with `NULL` assignment,
but assigning `NULL` to a list index deletes entries:


```r
example_list <- structure(as.list(1:10), names = letters[1:10])
example_list[1:3] <- NULL
example_list
```

```
## $d
## [1] 4
## 
## $e
## [1] 5
## 
## $f
## [1] 6
## 
## $g
## [1] 7
## 
## $h
## [1] 8
## 
## $i
## [1] 9
## 
## $j
## [1] 10
```

To store a `NULL` entry, you need to create a list first:


```r
example_list[c(1, 4)] <- list(NULL)
example_list
```

```
## $d
## NULL
## 
## $e
## [1] 5
## 
## $f
## [1] 6
## 
## $g
## NULL
## 
## $h
## [1] 8
## 
## $i
## [1] 9
## 
## $j
## [1] 10
```

You might expect, based on the analogy with vectors, that
`[[` lets you avoid the `list` invocation, but it also deletes entries:


```r
example_list[[1]] <- NULL
example_list
```

```
## $e
## [1] 5
## 
## $f
## [1] 6
## 
## $g
## NULL
## 
## $h
## [1] 8
## 
## $i
## [1] 9
## 
## $j
## [1] 10
```

The `[[` function accepts vector arguments for lists of lists:

```r
example_list <- lapply(seq_len(10), function (x) as.list(x ^ seq_len(x)))
example_list[[c(4, 3)]]
```

```
## [1] 64
```

Lists can have dimension attributes (I've never seen anyone use this in
real code) and then you can use array indexing:

```r
example_list <- array(as.list(1:12), c(2, 2, 3))
example_list[2, 1, 3]
```

```
## [[1]]
## [1] 10
```

```r
example_list[[2, 1, 3]]
```

```
## [1] 10
```

### Closures and garbage collection

I learned about this issue on
[the Win-Vector Blog](http://www.win-vector.com/blog/2015/03/using-closures-as-objects-in-r/).
Closures in R are not really closures because they retain the entire
environment of their definition, including things you wouldn't expect
to be accessible:


```r
make_accumulator <- function () {
    garbage <- rnorm(65536)
    x <- 0
    function (y) {
        x <<- x + sum(y)
        x
    }
}

test_accum <- make_accumulator()
test_accum(5)
```

```
## [1] 5
```

```r
test_accum(10)
```

```
## [1] 15
```

```r
object.size(test_accum)
```

```
## 5736 bytes
```

So far, so good. But in the documentation for `object.size`, we see

     Associated space (e.g., the environment of a function and what the
     pointer in a 'EXTPTRSXP' points to) is not included in the
     calculation.

In fact, we can still get `garbage`:


```r
str(get("garbage", environment(test_accum)))
```

```
##  num [1:65536] -0.377 -0.685 0.831 1.51 0.556 ...
```

The true size is exposed with `serialize`:


```r
object.size(serialize(test_accum, NULL))
```

```
## 530784 bytes
```

### Data frames

You can store matrices and arrays in data frames. However, you can't
create data frames with `data.frame` or `data_frame` that way:


```r
array_frame <- data.frame(x = 1:10)
array_frame$m <- array(rnorm(60), c(10, 3, 2))

names(array_frame)
```

```
## [1] "x" "m"
```

```r
names(data.frame(x = 1:10, m = array(rnorm(60), c(10, 3, 2))))
```

```
## [1] "x"   "m.1" "m.2" "m.3" "m.4" "m.5" "m.6"
```

Also, the `[` operator doesn't work with these the way you might
expect:



```r
list(array_frame[1, "m"], array_frame$m[1, , ])
```

```
## [[1]]
## [1] -1.202653
## 
## [[2]]
##            [,1]       [,2]
## [1,] -1.2026530 -0.7556250
## [2,] -0.3581968 -0.9711227
## [3,] -0.3830408 -0.6716219
```

The `dplyr` package will not let you work with these exotic data
frames, and `data.table` won't let you create them (as far as I know).
You could convert the column to a list:


```r
array_frame$m <- array_frame %$%
    vapply(seq_len(nrow(m)), function (r) list(m[r, , ]), list(1))
array_frame[1, "m"]
```

```
## [[1]]
##            [,1]       [,2]
## [1,] -1.2026530 -0.7556250
## [2,] -0.3581968 -0.9711227
## [3,] -0.3830408 -0.6716219
```

In general, lists in data frames are a very useful idiom. However, in
this case by using a boxed structure you lose all the advantages of
array storage that you had in the first place.

## Vectorized functions

Always use `rowSums`, `colSums`, `rowMeans`, and `colMeans` instead of
the equivalent `apply` call. Oddly enough, this seems to be it. There
is no vectorized version of `cumsum`.

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

This is even confusing in the discussion of `|:` in
[J for C programmers](http://www.jsoftware.com/help/jforc/contents.htm):
"I wish I could give you an intuitive definition but I can't."

Incidentally, if you have an integer permutation `v`, then `order(v)`
is its inverse (i.e., if `v` is the integers 1 through `n` rearranged,
then `v[order(v)] == order(v)[v] == seq_len(n)`).

## Apply and margins

Be careful with `apply(X, MARGIN, FUN, ...)` when `FUN` returns a
vector. The documentation explains:

    If each call to 'FUN' returns a vector of length 'n', then 'apply'
    returns an array of dimension 'c(n, dim(X)[MARGIN])' if 'n > 1'.
    If 'n' equals '1', 'apply' returns a vector if 'MARGIN' has length
    1 and an array of dimension 'dim(X)[MARGIN]' otherwise.  If 'n' is
    '0', the result has length 0 but not necessarily the 'correct'
    dimension.

In fact, if `MARGIN` is a permutation of the indices and `FUN` is the
identity, `apply` acts the same as `aperm`:


```r
test_array <- array(rnorm(48), c(2, 2, 4, 3))
assert_that(all(aperm(test_array, c(4, 2, 1, 3)) ==
                    apply(test_array, c(4, 2, 1, 3), function (x) x)))
```

What happens to the non-`MARGIN` axes? The key to remember is that
`apply` doesn't act in place, so when you use the row as the margin,
the result appears transposed:


```r
test_matrix <- matrix(1:12, 3, 4)
list(original = test_matrix,
     row_margin = apply(test_matrix, 1, cumsum),
     col_margin = apply(test_matrix, 2, cumsum))
```

```
## $original
##      [,1] [,2] [,3] [,4]
## [1,]    1    4    7   10
## [2,]    2    5    8   11
## [3,]    3    6    9   12
## 
## $row_margin
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    5    7    9
## [3,]   12   15   18
## [4,]   22   26   30
## 
## $col_margin
##      [,1] [,2] [,3] [,4]
## [1,]    1    4    7   10
## [2,]    3    9   15   21
## [3,]    6   15   24   33
```

This is in contrast to, say, J (which I don't know at all!). J
has fewer surprises, but is difficult to read:

```
   |: 1 + i. 4 3
1 4 7 10
2 5 8 11
3 6 9 12
   +/\ |: 1 + i. 4 3
1  4  7 10
3  9 15 21
6 15 24 33
   +/\"1 |: 1 + i. 4 3
1 5 12 22
2 7 15 26
3 9 18 30
   2 # |: 1 + i. 4 3
1 4 7 10
1 4 7 10
2 5 8 11
2 5 8 11
3 6 9 12
3 6 9 12
   2 #"1 |: 1 + i. 4 3
1 1 4 4 7 7 10 10
2 2 5 5 8 8 11 11
3 3 6 6 9 9 12 12
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
This is one task that's awkward with `dplyr`:


```r
local({
    x <- data.table(z = with_seed(12345, rnorm(1000)), key = "z")
    x_breaks <- data.table(z = seq(min(x$z), max(x$z), length.out = 10), key = "z")
    x_breaks[x, .(count = .N), roll = TRUE, by = z]
})
```



|          z| count|
|----------:|-----:|
| -2.7783255|    21|
| -2.0995412|    52|
| -1.4207569|   132|
| -0.7419726|   249|
| -0.0631883|   260|
|  0.6155961|   186|
|  1.2943804|    68|
|  1.9731647|    28|
|  2.6519490|     3|
|  3.3307333|     1|

After working with `dplyr` and `data.table` for a while, though, you
forget what base R has to offer:

```r
with_seed(12345, rnorm(1000)) %>%
    cut(breaks = c(seq(min(.), max(.), length.out = 10), Inf),
        include.lowest = TRUE, right = FALSE) %>%
    table
```



| [-2.78,-2.1)| [-2.1,-1.42)| [-1.42,-0.742)| [-0.742,-0.0632)| [-0.0632,0.616)| [0.616,1.29)| [1.29,1.97)| [1.97,2.65)| [2.65,3.33)| [3.33,Inf]|
|------------:|------------:|--------------:|----------------:|---------------:|------------:|-----------:|-----------:|-----------:|----------:|
|           21|           52|            132|              249|             260|          186|          68|          28|           3|          1|
The semantics of this operation are slightly different than a rolling
join (which is more efficient when everything is sorted, but that's
another issue…).

## Linear algebra

### Cholesky decompositions

For some reason, R has `chol2inv` but not `chol2solve`.


```r
chol2solve <- function (R, b) {
    assert_that(is.matrix(R))
    if (to_drop <- is.null(dim(b))) b <- matrix(b)
    assert_that(nrow(b) == nrow(R))
    pivot <- attr(R, "pivot")
    w <- if (is.null(pivot)) {
        backsolve(R, backsolve(R, b, transpose = TRUE))
    } else {
        assert_that(attr(R, "rank") == nrow(R))
        inv_pivot <- order(pivot)
        backsolve(R, backsolve(R, b[pivot, , drop = FALSE],
                               transpose = TRUE))[inv_pivot, ]
    }
    if (to_drop) w <- drop(w)
    w
}

test_that("chol2solve handles linear systems", {
    m <- matrix(c(14, -11, 5, -11, 17, -5, 5, -5, 13), 3, 3)
    b <- c(1, 2, 5)
    expect_that(chol2solve(chol(m), b), equals(solve(m, b)))
})

test_that("chol2solve handles pivots", {
    m <- matrix(c(14, -11, 5, -11, 17, -5, 5, -5, 13), 3, 3)
    b <- c(1, 2, 5)
    m_ch <- chol(m, pivot = TRUE)
    expect_that(attr(m_ch, "pivot"), equals(c(2, 3, 1)))
    expect_that(chol2solve(m_ch, b), equals(solve(m, b)))
})
```

It almost goes without saying that you should never invert matrices
(never write `solve(a)`, and always use `solve(a, v)` in favor of
`solve(a) %*% v`). The `chol2inv` function makes an appearance in
`summary.lm` where it is used to compute standard errors of regression
coefficients.

### crossprod

Use `crossprod(x, y)` in favor of `t(x) %*% y`. There is an analogous
function `tcrossprod(x, y)` to replace `x %*% t(y)`.

### QR decompositions

The
[R documentation on QR decompositions](https://stat.ethz.ch/R-manual/R-devel/library/base/html/qr.html)
is worth reading. But it never, as far as I know, explains the content
of the `qraux` vector in the output of `qr`. At some point I dug into
the source code for the QR decomposition routines (look at, say,
[`src/appl/dqrdc2.f`](https://github.com/wch/r-source/blob/trunk/src/appl/dqrdc2.f)),
which contain an ominous comment that
[dates back to 1997](https://svn.r-project.org/R/tags/R-0-60/src/appl.f/dqrdc2.f):

```fortran
c     i am very nervous about modifying linpack code in this way.
c     if you are a computational linear algebra guru and you really
c     understand how to solve this problem please feel free to
c     suggest improvements to this code.
```

As it happens, `qraux` stores different data depending on whether you
use LAPACK or LINPACK routines. The i-th Householder vector is stored
in the last n - i entries of the i-th column of `qr`. The first
i - 1 entries of the Householder vector are zero. Here's where the
LINPACK and LAPACK decompositions in R differ:

* In LINPACK (the default), the i-th entry of the Householder vector
  is stored in `qraux[i]` and β is `1 / qraux[i]`.
* When you use `LAPACK = TRUE`, the i-th entry of the Householder
  vector is 1 and β is `qraux[i]`.

In R notation, the Householder transformation associated to a pair
`(beta, v)` is the function
```r
function (w) w - beta * v %*% crossprod(v, w)
```
For details on the algorithm, see
[Golub and Van Loan](http://www.cs.cornell.edu/cv/GVL4/golubandvanloan.htm).

## Math

### Logit and inverse logit

For the inverse logit, there is no need to write `1 / (1 + exp(-x))`.
Use `plogis` (in the `stats` package, which is loaded by default) or
`boot::inv.logit` (which just calls `plogis`). For the logit, use
`stats::qlogis` or `boot::logit` (which is just an alias). There is no
real numerical reason to do this, but the internal R functions are
guaranteed to handle `NaN` and infinity correctly, and the functions
are more readable.

### Computing upper tails

Never write `1 - pfoobar(...)`. All of the
distribution functions accept an optional `lower.tail`
so `1 - pfoobar(...)` can be replaced by `pfoobar(...,
lower.tail = FALSE)`. It looks like more typing, but it is more
precise due to the potential catastrophic cancellation:


```r
c(1 - pnorm(10), pnorm(10, lower.tail = FALSE))
```

```
## [1] 0.000000e+00 7.619853e-24
```

If you find yourself writing `log(pfoobar(...))`, use `pfoobar(...,
log = TRUE)` instead.

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
pp <- function (x) page(x, method = "print", max = 99999L)
```

## ESS and EMACS

This isn't really R stuff per se, but my notes editing R code.
Here's my ESS config from (some version of) my
[`.emacs`](https://github.com/sbisaacson/literate-emacs):
```elisp
(defun sbi/ess-setup ()
  "Set up ESS customizations."
  (ess-toggle-underscore nil)
  (ess-add-style 'sbi-custom
                 '((ess-indent-level . 4)
                   (ess-first-continued-statement-offset . 4)
                   (ess-continued-statement-offset . 0)
                   (ess-brace-offset . 0)
                   (ess-arg-function-offset . 4)
                   (ess-arg-function-offset-new-line . '(4))
                   (ess-expression-offset . 4)
                   (ess-else-offset . 0)
                   (ess-close-brace-offset . 0))))

(use-package ess-site
  :ensure ess
  :commands R
  :mode (("\\.[rR]\\'" . R-mode)
         ("\\.Rnw\\'" . Rnw-mode))
  :config (sbi/ess-setup))
```

Some people really don't like underscores because you have to press
the shift key to type them, and [ESS](http://ess.r-project.org/) has
helpfully bound that to `<-`, so you have to hit shift-hyphen twice.
Or you could just disable this feature: that's the
`(ess-toggle-underscore nil)`.
[Google](https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#identifiers)
recommends using full stops or camelCase. But full stops in names
interfere with the S3 class system.
[Hadley Wickham](http://adv-r.had.co.nz/Style.html) recommends
underscores, but beware of this convention. Some of his functions only
differ in name from core R by replacing `.` with `_`, but differ in
behavior—`data.frame` has `stringsAsFactors = TRUE` by default, while
`dplyr::data_frame` has `stringsAsFactors = FALSE` (as it should have
been). I go for underscores on this one. There are no S3 surprises and
`We_the_people_of_the_United_States` is a lot more readable than
`inOrderToFormAMorePerfectUnion`.

ESS by default will indent magrittr/dplyr pipelines in a charming
echelon fashion:

```r
mtcars %>%
    group_by(cyl, gear) %>%
        summarise(mean(wt), mean(hp)) %>%
            ungroup %>%
                arrange(`mean(wt)`)
```

You have to mess with `ess-continued-statement-offset` and
`ess-first-continued-statement-offset` to get something sane like

```r
mtcars %>%
    group_by(cyl, gear) %>%
    summarise(mean(wt), mean(hp)) %>%
    ungroup %>%
    arrange(`mean(wt)`)
```

## Useful links

1. Hadleyverse:
  1. [Advanced R](http://adv-r.had.co.nz/)
  2. [R packages](http://r-pkgs.had.co.nz/)
  3. [ggplot2](http://docs.ggplot2.org/current/)
  4. [github link](http://github.com/hadley)
2. [R project](http://www.r-project.org/)
  1. [R internals](http://cran.r-project.org/doc/manuals/r-release/R-ints.html)
  2. [R language definition](http://cran.r-project.org/doc/manuals/r-release/R-lang.html)
  3. [Writing R extensions](http://cran.r-project.org/doc/manuals/r-release/R-exts.html)
  4. [github R mirror](https://github.com/wch/r-source)
3. [Rcpp](http://www.rcpp.org/)
  1. [Armadillo](http://arma.sourceforge.net/)
  2. [Rcpp documentation](http://dirk.eddelbuettel.com/code/rcpp.html)
  3. [RcppArmadillo](http://dirk.eddelbuettel.com/code/rcpp.armadillo.html)
  4. [RcppEigen](http://www.jstatsoft.org/v52/i05)
4. Netlib
  1. [BLAS](http://www.netlib.org/blas/)
  2. [LAPACK](http://www.netlib.org/lapack/)
5. data.table
  1. [wiki](https://github.com/Rdatatable/data.table/wiki)
  2. [cheat sheet](https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf)
6. Useful packages
  1. [zoo](http://cran.r-project.org/web/packages/zoo/index.html)
  2. [Yihui Xie's github](https://github.com/yihui)
7. Stan (Bayesian modeling)
  1. [Stan project](http://mc-stan.org/)
  2. [Manual](http://mc-stan.org/manual.html)
