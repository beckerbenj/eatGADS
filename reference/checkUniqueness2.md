# Check uniqueness of a variable.

Function to check if a variable is unique for all cases of an identifier
variable. This is a fast and more efficient version of
[`checkUniqueness`](https://beckerbenj.github.io/eatGADS/reference/checkUniqueness.md)
which always returns a logical, non missing value of length one.

## Usage

``` r
checkUniqueness2(GADSdat, varName, idVar, impVar)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varName:

  Single string containing the variable name for which the check should
  be performed.

- idVar:

  Single string containing the name of the identifier variable.

- impVar:

  Single string containing the name of the imputation number.

## Value

Returns a logical of length one.

## Details

For example if missing values are multiple imputed and data is stored in
a long format, checking the uniqueness of a variable within an
identifier can be tricky. This function automates this task via
reshaping the data into wide format and testing equality among the
reshaped variables. Similar functionality (via matrices) is covered by
`lme4::isNested`, which is more general and performs similarly.

## Examples

``` r
## create an example GADSdat
l <- 1000
long_df <- data.table::data.table(id = sort(rep(1:l, 15)),
                               v1 = sort(rep(1:l, 15)),
                                 imp = rep(1:15, l))
gads <- import_DF(long_df)
## check uniqueness
checkUniqueness2(gads, varName = "v1", idVar = "id", impVar = "imp")
#> [1] TRUE
```
