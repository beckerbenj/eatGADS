# Check a `GADSdat` for compatibility with `Stata`.

This function performs all relevant checks to assess if a `GADSdat`
complies with all of Stata's dataset requirements. Run this before
exporting a dataset as `.dta`, using
[`write_stata`](https://beckerbenj.github.io/eatGADS/reference/write_spss.md).

## Usage

``` r
check4Stata(GADSdat, version = c("Stata", "Stata 19/BE", "Stata 19/MP"))
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- version:

  Optional single string to request checks for a specific Stata version
  (see details).

## Value

Either `NULL` if all checks are passed successfully, or a `list` of all
check results (see details for explanations of the keywords) if any
problem was detected.

## Details

Specifically, the following requirements are tested:

|                               |                                                                                                                                                  |
|-------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| `dots_in_varNames`\*          | Variable names do not contain dots ([`checkVarNames`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)).                         |
| `special_chars_in_varNames`\* | Variable names do not contain special characters.                                                                                                |
| `varName_length`\*            | Variable names are not longer than the specific limit ([`checkVarNames`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)).      |
| `labeled_fractionals`\*       | There are no labeled fractional values ([`checkLabeledFractionals`](https://beckerbenj.github.io/eatGADS/reference/checkLabeledFractionals.md)). |
| `large_integers`\*            | All labeled values can be coerced `as.integer` ([`checkIntOverflow`](https://beckerbenj.github.io/eatGADS/reference/checkIntOverflow.md)).       |
| `varLabel_length`             | Variable labels are not longer than the specific limit ([`checkVarLabels`](https://beckerbenj.github.io/eatGADS/reference/checkValLabels.md)).   |
| `valLabel_length`             | Value labels are not longer than the specific limit ([`checkValLabels`](https://beckerbenj.github.io/eatGADS/reference/checkValLabels.md)).      |
| `long_strings`                | String variables do not contain string values that are longer than the specific limit.                                                           |
| `too_many_rows`\*             | The number of rows/observations does not exceed the specific limit.                                                                              |
| `too_many_cols`\*             | The number of columns/variables does not exceed the specific limit.                                                                              |

Not complying with the marked (\*) requirements will prevent a dataset
from being exported to a `.dta` file. Issues with unmarked requirements
will be solved automatically by truncating.

Limits to different aspects of the dataset vary between versions of the
software. By default (`version = "Stata"`), compliance with the limits
for `Stata 19/SE` is checked. Checks against the limits for
`Stata 19/BE` or `Stata 19/MP` can be requested by specifying `version`
with the corresponding string. For more details, see
[program_limits](https://beckerbenj.github.io/eatGADS/reference/program_limits.md).

## See also

Other dataset compliance checks:
[`check4SPSS()`](https://beckerbenj.github.io/eatGADS/reference/check4SPSS.md)

## Examples

``` r
check4Stata(pisa)
#> $dots_in_varNames
#> character(0)
#> 
#> $special_chars_in_varNames
#> character(0)
#> 
#> $varName_length
#> character(0)
#> 
#> $labeled_fractionals
#> [1] varName  value    missings empty   
#> <0 rows> (or 0-length row.names)
#> 
#> $large_integers
#> [1] varName  value    missings empty    rownum  
#> <0 rows> (or 0-length row.names)
#> 
#> $varLabel_length
#>     varName                                    varLabel length unit
#> 1 attitud_a Attitude towards School - Does Little to...     81 char
#> 
#> $valLabel_length
#> [1] varName  value    valLabel length   unit     empty   
#> <0 rows> (or 0-length row.names)
#> 
#> $long_strings
#> [1] varName string 
#> <0 rows> (or 0-length row.names)
#> 
#> $too_many_rows
#> [1] 0
#> 
#> $too_many_cols
#> [1] 0
#> 
```
