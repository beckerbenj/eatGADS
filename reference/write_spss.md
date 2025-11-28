# Write a `GADSdat` object to a file

Write a `GADSdat` object, which contains meta information as value and
variable labels to an `SPSS` file (`sav`) or `Stata` file (`dta`). See
'details' for some important limitations.

## Usage

``` r
write_spss(GADSdat, filePath)

write_stata(GADSdat, filePath)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- filePath:

  Path of `sav` file to write.

## Value

Writes file to disc, returns `NULL`.

## Details

The provided functionality relies on `havens`
[`write_sav`](https://haven.tidyverse.org/reference/read_spss.html) and
[`write_dta`](https://haven.tidyverse.org/reference/read_dta.html)
functions.

Currently known limitations for `write_spss` are:

- a\) value labels for long character variables (\> `A10`) are dropped,

- b\) under specific conditions very long character variables (\>
  `A254`) are incorrectly displayed as multiple character variables in
  `SPSS`,

- c\) exporting date or time variables is currently not supported,

- d\) missing tags are slightly incompatible between `SPSS` and
  `eatGADS` as `eatGADS` supports unlimited discrete missing tags (but
  no range of missing tags) and `SPSS` only supports up to three
  discrete missing tags or ranges of missing tags. For this purpose, if
  a variable is assigned more than three discrete missing tags,
  `write_spss()` (more precisely
  [`export_tibble`](https://beckerbenj.github.io/eatGADS/reference/export_tibble.md))
  performs a silent conversion of the discrete missing tags into a
  missing range. If this conversion affects other value labels or values
  in the data not tagged as missing, an error is issued.

Currently known limitations for `write_stata` are:

- a\) Variable format is dropped,

- b\) missing codes are dropped.

## Examples

``` r
# write to spss
tmp <- tempfile(fileext = ".sav")
write_spss(pisa, tmp)
#> NULL

# write to stata
tmp <- tempfile(fileext = ".dta")
write_stata(pisa, tmp)
#> Warning: Missing codes and variable formats are dropped when writing to '.dta'.
#> NULL
```
