# Assimilate value labels.

Assimilate all value labels of multiple variables as part of a `GADSdat`
or `all_GADSdat` object.

## Usage

``` r
assimilateValLabels(GADSdat, varNames, lookup = NULL)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varNames:

  Character string of a variable name.

- lookup:

  Lookup `data.frame`.

## Value

Returns the `GADSdat` object with changed meta data and recoded values.

## Details

Assimilation can be performed using all existing value labels or a
lookup table containing at least all existing value labels. Missing
codes are reused based on the meta data of the first variable in
`varNames`.

## Examples

``` r
# Example data set
facs_df <- data.frame(id = 1:3, fac1 = c("Eng", "Aus", "Ger"),
                      fac2 = c("Ger", "Franz", "Ita"),
                      fac3 = c("Kor", "Chi", "Alg"),
                      stringsAsFactors = TRUE)
facs_gads <- import_DF(facs_df)

assimilateValLabels(facs_gads, varNames = paste0("fac", 1:3))
#> $dat
#>   id fac1 fac2 fac3
#> 1  1    4    6    8
#> 2  2    2    5    3
#> 3  3    6    7    1
#> 
#> $labels
#>    varName varLabel format display_width labeled value valLabel missings
#> 1       id     <NA>   <NA>            NA      no    NA     <NA>     <NA>
#> 2     fac1     <NA>  F10.0            NA     yes     1      Alg    valid
#> 3     fac1     <NA>  F10.0            NA     yes     2      Aus    valid
#> 4     fac1     <NA>  F10.0            NA     yes     3      Chi    valid
#> 5     fac1     <NA>  F10.0            NA     yes     4      Eng    valid
#> 6     fac1     <NA>  F10.0            NA     yes     5    Franz    valid
#> 7     fac1     <NA>  F10.0            NA     yes     6      Ger    valid
#> 8     fac1     <NA>  F10.0            NA     yes     7      Ita    valid
#> 9     fac1     <NA>  F10.0            NA     yes     8      Kor    valid
#> 10    fac2     <NA>  F10.0            NA     yes     1      Alg    valid
#> 11    fac2     <NA>  F10.0            NA     yes     2      Aus    valid
#> 12    fac2     <NA>  F10.0            NA     yes     3      Chi    valid
#> 13    fac2     <NA>  F10.0            NA     yes     4      Eng    valid
#> 14    fac2     <NA>  F10.0            NA     yes     5    Franz    valid
#> 15    fac2     <NA>  F10.0            NA     yes     6      Ger    valid
#> 16    fac2     <NA>  F10.0            NA     yes     7      Ita    valid
#> 17    fac2     <NA>  F10.0            NA     yes     8      Kor    valid
#> 18    fac3     <NA>  F10.0            NA     yes     1      Alg    valid
#> 19    fac3     <NA>  F10.0            NA     yes     2      Aus    valid
#> 20    fac3     <NA>  F10.0            NA     yes     3      Chi    valid
#> 21    fac3     <NA>  F10.0            NA     yes     4      Eng    valid
#> 22    fac3     <NA>  F10.0            NA     yes     5    Franz    valid
#> 23    fac3     <NA>  F10.0            NA     yes     6      Ger    valid
#> 24    fac3     <NA>  F10.0            NA     yes     7      Ita    valid
#> 25    fac3     <NA>  F10.0            NA     yes     8      Kor    valid
#> 
#> attr(,"class")
#> [1] "GADSdat" "list"   
```
