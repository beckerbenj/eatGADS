# Inspect meta data differences in a variable.

Inspect meta data differences within a single `GADSdat` or between two
`GADSdat` objects or `GADSdat` data bases regarding a specific variable.

## Usage

``` r
inspectMetaDifferences(
  GADSdat,
  varName,
  other_GADSdat = GADSdat,
  other_varName = varName
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- varName:

  A character vector of length 1 containing the variable name.

- other_GADSdat:

  A second `GADSdat` object. If omitted, it is assumed that both
  variables are part of the first `GADSdat`.

- other_varName:

  A character vector of length 1 containing the other variable name. If
  omitted, it is assumed that both variables have identical names (as
  supplied in `varName`).

## Value

A list.

## Details

Two `GADSdat` objects can be compared using
[`equalGADS`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md).
If meta data differences for specific variables in the two objects
occur, these variables can be further inspected using
`inspectMetaDifferences`. For data-level differences for a specific
variable, see
[`inspectDifferences`](https://beckerbenj.github.io/eatGADS/reference/inspectDifferences.md).

## Examples

``` r
# create a second GADS with different meta data
pisa2 <- pisa
pisa2 <- changeVarLabels(pisa2, varName = "sameteach", varLabel = "Same math teacher")
pisa2 <- recodeGADS(pisa2, varName = "sameteach", oldValues = c(1, 2), newValues = c(0, 1))

# inspect via equalGADS()
equalGADS(pisa, pisa2)
#> $names_not_in_1
#> character(0)
#> 
#> $names_not_in_2
#> character(0)
#> 
#> $data_differences
#> [1] "sameteach"
#> 
#> $data_nrow
#> [1] "all.equal"
#> 
#> $meta_data_differences
#> [1] "sameteach"
#> 

# inspect via inspectMetaDifferences()
inspectMetaDifferences(GADSdat = pisa, varName = "sameteach", other_GADSdat = pisa2)
#> $varDiff
#>                         GADSdat_varLabel GADSdat_format other_GADSdat_varLabel
#> 1 Same math teacher in both school years           F8.0      Same math teacher
#>   other_GADSdat_format
#> 1                 F8.0
#> 
#> $valDiff
#>   value GADSdat_valLabel GADSdat_missings other_GADSdat_valLabel
#> 1     0             <NA>             <NA>                     No
#> 2     1               No            valid                    Yes
#> 3     2              Yes            valid                   <NA>
#>   other_GADSdat_missings
#> 1                  valid
#> 2                  valid
#> 3                   <NA>
#> 
```
