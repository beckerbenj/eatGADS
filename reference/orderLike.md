# Order the variables in a `GADSdat`.

Order the variables in a `GADSdat` according to a character vector. If
there are discrepancies between the two sets, a warning is issued.

## Usage

``` r
orderLike(GADSdat, newOrder)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- newOrder:

  A character vector containing the order of variables.

## Value

Returns a `GADSdat` object.

## Details

The variables in the `dat` and in the `labels` section are ordered.
Variables not contained in the character vector are moved to the end of
the data.
