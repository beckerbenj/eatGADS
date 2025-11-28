# Compare two GADS.

Compare multiple variables of two `GADSdat` or `all_GADSdat` objects.

## Usage

``` r
compareGADS(
  GADSdat_old,
  GADSdat_new,
  varNames,
  output = c("list", "data.frame", "aggregated")
)
```

## Arguments

- GADSdat_old:

  `GADSdat` object imported via `eatGADS`.

- GADSdat_new:

  `GADSdat` object imported via `eatGADS`.

- varNames:

  Character string of variable names to be compared.

- output:

  How should the output be structured?

## Value

Returns either a list with `"all equal"` and `data.frames` or a single
`data.frame`.

## Details

Returns `"all equal"` if the variable is identical across the objects or
a `data.frame` containing a frequency table with the values which have
been changed. Especially useful for checks after recoding.

## Examples

``` r
# Recode a GADS
pisa2 <- recodeGADS(pisa, varName = "schtype",
                        oldValues = 3, newValues = 9)
pisa2 <- recodeGADS(pisa2, varName = "language",
                        oldValues = 1, newValues = 15)

# Compare
compareGADS(pisa, pisa2,
            varNames = c("ganztag", "schtype", "language"), output = "list")
#> $ganztag
#> [1] "all equal"
#> 
#> $schtype
#>   value frequency                                  valLabel missings
#> 1     3       111 schools with several courses of education    valid
#> 
#> $language
#>   value frequency valLabel missings
#> 1     1       436   German    valid
#> 
compareGADS(pisa, pisa2,
            varNames = c("ganztag", "schtype", "language"), output = "data.frame")
#>   variable value frequency                                  valLabel missings
#> 1  schtype     3       111 schools with several courses of education    valid
#> 2 language     1       436                                    German    valid
compareGADS(pisa, pisa2,
             varNames = c("ganztag", "schtype", "language"), output = "aggregated")
#>   value                                  valLabel missings
#> 1     3 schools with several courses of education    valid
#> 2     1                                    German    valid
```
