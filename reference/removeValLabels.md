# Remove value labels and missing tags.

Remove meta data for specific values (`value`) of a single variable
(`varName`). This includes value labels and missings tags.

## Usage

``` r
removeValLabels(GADSdat, varName, value, valLabel = NULL)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varName:

  Character string of a variable name.

- value:

  Numeric values.

- valLabel:

  \[optional\] Regular expressions in the value labels corresponding to
  `value`.

## Value

Returns the `GADSdat` object with changed meta data.

## Details

If the argument `valLabel` is provided, the function checks for `value`
and `valLabel` pairs in the meta data that match both arguments.

## Examples

``` r
# Remove a label based on value
extractMeta(pisa, "schtype")
#>   varName     varLabel format display_width labeled value
#> 5 schtype School track   F8.0            NA     yes     1
#> 6 schtype School track   F8.0            NA     yes     2
#> 7 schtype School track   F8.0            NA     yes     3
#>                                    valLabel missings
#> 5                Gymnasium (academic track)    valid
#> 6                                Realschule    valid
#> 7 schools with several courses of education    valid
pisa2 <- removeValLabels(pisa, varName = "schtype", value = 1)
extractMeta(pisa2, "schtype")
#>   varName     varLabel format display_width labeled value
#> 6 schtype School track   F8.0            NA     yes     2
#> 7 schtype School track   F8.0            NA     yes     3
#>                                    valLabel missings
#> 6                                Realschule    valid
#> 7 schools with several courses of education    valid

# Remove multiple labels based on value
extractMeta(pisa, "schtype")
#>   varName     varLabel format display_width labeled value
#> 5 schtype School track   F8.0            NA     yes     1
#> 6 schtype School track   F8.0            NA     yes     2
#> 7 schtype School track   F8.0            NA     yes     3
#>                                    valLabel missings
#> 5                Gymnasium (academic track)    valid
#> 6                                Realschule    valid
#> 7 schools with several courses of education    valid
pisa3 <- removeValLabels(pisa, varName = "schtype", value = 1:3)
extractMeta(pisa3, "schtype")
#>   varName     varLabel format display_width labeled value valLabel missings
#> 5 schtype School track   F8.0            NA      no    NA     <NA>     <NA>

# Remove multiple labels based on value - valLabel combination
extractMeta(pisa, "schtype")
#>   varName     varLabel format display_width labeled value
#> 5 schtype School track   F8.0            NA     yes     1
#> 6 schtype School track   F8.0            NA     yes     2
#> 7 schtype School track   F8.0            NA     yes     3
#>                                    valLabel missings
#> 5                Gymnasium (academic track)    valid
#> 6                                Realschule    valid
#> 7 schools with several courses of education    valid
pisa4 <- removeValLabels(pisa, varName = "schtype",
                        value = 1:3, valLabel = c("Gymnasium", "other", "several courses"))
extractMeta(pisa4, "schtype")
#>   varName     varLabel format display_width labeled value   valLabel missings
#> 6 schtype School track   F8.0            NA     yes     2 Realschule    valid
```
