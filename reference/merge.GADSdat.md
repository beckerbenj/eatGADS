# Merge two `GADSdat` objects into a single `GADSdat` object.

Is a secure way to merge the data and the meta data of two `GADSdat`
objects. Currently, only limited merging options are supported.

## Usage

``` r
# S3 method for class 'GADSdat'
merge(
  x,
  y,
  by,
  all = TRUE,
  all.x = all,
  all.y = all,
  missingValue = NULL,
  missingValLabel = NULL,
  ...
)
```

## Arguments

- x:

  `GADSdat` object imported via `eatGADS`.

- y:

  `GADSdat` object imported via `eatGADS`.

- by:

  A character vector.

- all:

  A character vector (either a full join or an inner join).

- all.x:

  See merge.

- all.y:

  See merge.

- missingValue:

  A numeric value that is used to replace missing values introduced
  through the merge.

- missingValLabel:

  The value label that is assigned to all variables into which
  `missingValue` is inserted.

- ...:

  Further arguments are currently not supported but have to be included
  for `R CMD` checks.

## Value

Returns a `GADSdat` object.

## Details

If there are duplicate variables (except the variables specified in the
`by` argument), these variables are removed from y. The meta data is
joined for the remaining variables via `rbind`.

The function supports automatically recoding missing values created
through merging with a designated missing code (`missingValue`) and a
value label (`missingValLabel`).
