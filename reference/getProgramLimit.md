# Get program specific limits

Get the (most restrictive) limit that `SPSS` and/or `Stata` imposes on a
specific aspect of a dataset.

## Usage

``` r
getProgramLimit(
  program = c("SPSS", "Stata", "Stata 19/BE", "Stata 19/MP"),
  component = c("varNames", "varLabels", "valLabels", "stringvars", "nrows", "ncols")
)
```

## Arguments

- program:

  Character vector of the programs/program version that should be
  considered.

- component:

  Single string. Which limits should be returned?

## Value

A list of two elements: `value` (numeric size of the limit) and `unit`
("char", "byte", or "generic").

## Details

`SPSS` and `Stata` impose different limits to different components of
their datasets, e.g., on the length of variable names. For details and a
list of relevant limits, see
[`program_limits`](https://beckerbenj.github.io/eatGADS/reference/program_limits.md).
Additionally, limits may vary between software versions. This primarily
applies to `Stata`'s product tiers, but also for (very) old `SPSS`
versions.

For `program`, `"SPSS"` and `"Stata"` imply `SPSS 30` and `Stata 19/SE`,
respectively, as these are the most relevant version among the ones
implemented here.

If more than one program/version name is given in `program`, the most
restrictive limit will be returned.

## See also

Other dataset compliance checks:
[`checkIntOverflow()`](https://beckerbenj.github.io/eatGADS/reference/checkIntOverflow.md),
[`checkLabeledFractionals()`](https://beckerbenj.github.io/eatGADS/reference/checkLabeledFractionals.md)

## Examples

``` r
# Show all implemented limits
program_limits
#>     component     program         value    unit
#> 1    varNames        SPSS            64    byte
#> 2    varNames       Stata            32    char
#> 3    varNames Stata 19/BE            32    char
#> 4    varNames Stata 19/MP            32    byte
#> 5   varLabels        SPSS           256 generic
#> 6   varLabels       Stata            80 generic
#> 7   varLabels Stata 19/BE            80    char
#> 8   varLabels Stata 19/MP            80    char
#> 9   valLabels        SPSS           120    char
#> 10  valLabels       Stata         30000    byte
#> 11  valLabels Stata 19/BE         30000 generic
#> 12  valLabels Stata 19/MP         30000 generic
#> 13 stringvars        SPSS         32767    char
#> 14 stringvars       Stata       2000000    char
#> 15 stringvars Stata 19/BE       2000000    char
#> 16 stringvars Stata 19/MP       2000000    byte
#> 17      nrows        SPSS    2147483647 generic
#> 18      nrows       Stata    2147483619 generic
#> 19      nrows Stata 19/BE    2147483619    char
#> 20      nrows Stata 19/MP 1099511627775    char
#> 21      ncols        SPSS    2147483647    char
#> 22      ncols       Stata         32767    byte
#> 23      ncols Stata 19/BE          2048 generic
#> 24      ncols Stata 19/MP        120000 generic

# Get the specific limit on variable name lengths under SPSS
getProgramLimit("SPSS", "varNames")
#> $value
#> [1] 64
#> 
#> $unit
#> [1] "byte"
#> 

# Get the variable name length limit a dataset has to adhere to to be compatible with
#  both SPSS and Stata 19/SE
getProgramLimit(c("Stata", "SPSS"), "varNames")
#> $value
#> [1] 32
#> 
#> $unit
#> [1] "char"
#> 
```
