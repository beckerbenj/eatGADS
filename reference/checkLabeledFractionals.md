# Check a `GADSdat` for labeled fractional values.

Check a `GADSdat` object for any occurrences of fractional values in its
metadata, including both "truly" labeled values and values tagged as
`missings`.

## Usage

``` r
checkLabeledFractionals(GADSdat)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

## Value

Returns a `data.frame`, listing the affected `varName`s, the labeled
fractional `value`s, their respective `missings` tags, and whether they
actually occur in the data (`empty`).

## Details

This function is mainly useful to ensure a data set can be saved as a
`.dta` file. Unlike, for example, SPSS, Stata only allows for integer
values (and so-called extended missing values) to be labeled ([Stata
manual:
12.6.3](https://www.stata.com/manuals/u12.pdf#u12.6.3Valuelabels)).
Trying to export (meta) data with labeled fractional values would
therefore cause problems and run into an error from `haven`'s
[`write_dta`](https://haven.tidyverse.org/reference/read_dta.html)
function.

## See also

Other dataset compliance checks:
[`checkIntOverflow()`](https://beckerbenj.github.io/eatGADS/reference/checkIntOverflow.md),
[`getProgramLimit()`](https://beckerbenj.github.io/eatGADS/reference/getProgramLimit.md)

## Examples

``` r
# Introduce a fractional value into meta data
pisa2 <- recodeGADS(GADSdat = pisa,
                    varName = "schtype",
                    oldValues = 2,
                    newValues = .5)
checkLabeledFractionals(pisa2)
#>   varName value missings empty
#> 1 schtype   0.5    valid FALSE
```
