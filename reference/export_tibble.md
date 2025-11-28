# Transform a `GADSdat` to a `tibble`

`haven`'s
[`read_spss`](https://haven.tidyverse.org/reference/read_spss.html)
stores data together with meta data (e.g. value and variable labels) in
a `tibble` with attributes on variable level. This function transforms a
`GADSdat` object to such a `tibble`.

## Usage

``` r
export_tibble(GADSdat)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

## Value

Returns a `tibble`.

## Details

This function is mainly intended for internal use. For further
documentation see also
[`write_spss`](https://beckerbenj.github.io/eatGADS/reference/write_spss.md).

## Examples

``` r
pisa_tbl <- export_tibble(pisa)
```
