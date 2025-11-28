# Recode Missings to `NA`

Recode Missings to `NA` according to missing labels in label
`data.frame`.

## Usage

``` r
miss2NA(GADSdat)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

## Value

Returns a `data.frame` with `NA` instead of missing codes.

## Details

Missings are imported as their values via
[`import_spss`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md).
Using the value labels in the labels `data.frame`, `miss2NA` recodes
these missings codes to `NA`. This function is mainly intended for
internal use.
