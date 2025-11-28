# Check a `GADSdat` for large labeled whole-number values.

Check a `GADSdat` object for any occurrences of labeled whole-number
values that would be too large for R to handle if they were coerced
[`as.integer()`](https://rdrr.io/r/base/integer.html).

## Usage

``` r
checkIntOverflow(GADSdat)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

## Value

Returns a `data.frame`, listing the affected `varName`s, the large
whole-number `value`s, their respective `missings` tags, and whether
they actually occur in the data (`empty`). The `rownum`s of the affected
rows in `GADSdat$labels` are also provided in a separate column as a
fail safe.

## Details

According to its documentation, R can only handle
[`integer`](https://rdrr.io/r/base/integer.html) values of up to
(roughly) \\\pm 2 \times 10^9\\ (2,147,483,647 to be exact; c.f.
[`.Machine`](https://rdrr.io/r/base/zMachine.html)`$integer.max`). This
restriction is relevant when exporting a `GADSdat` to `.dta` and only
when any value exceeding the limit is also labeled (or tagged as
missing). This is because Stata only accepts labeled *integer* (not
labeled *floating-point*; c.f.
[`checkLabeledFractionals()`](https://beckerbenj.github.io/eatGADS/reference/checkLabeledFractionals.md)
in this package) values. `haven`'s
[`write_dta`](https://haven.tidyverse.org/reference/read_dta.html)
function will therefore try to coerce any labeled values
[`as.integer()`](https://rdrr.io/r/base/integer.html). Unlabeled values,
however, will stay generic `numeric` values that have a higher limit.

## See also

Other dataset compliance checks:
[`checkLabeledFractionals()`](https://beckerbenj.github.io/eatGADS/reference/checkLabeledFractionals.md),
[`getProgramLimit()`](https://beckerbenj.github.io/eatGADS/reference/getProgramLimit.md)

## Examples

``` r
# Introduce a fractional value into meta data
pisa2 <- changeMissings(GADSdat = pisa,
                        varName = "schtype",
                        value = 9999999999,
                        missings = "miss")
checkIntOverflow(pisa2)
#>   varName value missings empty rownum
#> 1 schtype 1e+10     miss  TRUE      7
```
