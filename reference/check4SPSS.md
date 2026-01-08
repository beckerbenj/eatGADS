# Check `SPSS` Compliance of Meta Data

Function to check if variable names and labels, value labels and missing
codes comply with `SPSS` requirements for meta data.

## Usage

``` r
check4SPSS(GADSdat)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

## Value

Returns a list with the entries `"varNames_special"`,
`"varNames_length"`, `"varLabels"`, `"valLabels"` and `"missings"`.

## Details

The function measures the length of variable names (`"varNames_length"`,
maximum of 64 characters) variable labels (`"varLabels"`, maximum of 256
characters), value labels (`"valLabels"`, maximum of 120 characters).
Furthermore, missing codes are counted (`"missings"`, maximum of three
missing codes for character variables) and special characters are
flagged in variable names (`"varNames_special"`). Check results are
reported back on variable level, with the exception of `"valLabels"`,
which is a list with entries per violating variable.

## See also

Other dataset compliance checks:
[`check4Stata()`](https://beckerbenj.github.io/eatGADS/reference/check4Stata.md)

## Examples

``` r
# Change example data set (create a violating label)
pisa2 <- changeVarLabels(pisa, varName = "computer_age",
                        varLabel = paste(rep("3", 125), collapse = ""))

check4SPSS(pisa2)
#> $varNames_special
#> character(0)
#> 
#> $varNames_length
#> character(0)
#> 
#> $varLabels
#> character(0)
#> 
#> $valLabels
#> character(0)
#> 
#> $missings
#> character(0)
#> 
```
