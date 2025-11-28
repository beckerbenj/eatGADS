# Bind two `GADSdat` objects into a single `GADSdat` object by columns.

Is a secure way to `cbind` the data and the meta data of two `GADSdat`
objects. Currently, only limited merging options are supported.

## Usage

``` r
# S3 method for class 'GADSdat'
cbind(..., deparse.level = 1)
```

## Arguments

- ...:

  Multiple `GADSdat` objects imported via `eatGADS`.

- deparse.level:

  Argument is ignored in this method.

## Value

Returns a `GADSdat` object.

## Details

If there are duplicate variables (except the variables specified in the
`by` argument), these variables are removed from y. The meta data is
joined for the remaining variables via `rbind`.
