# Get Meta Data

Extract meta data (e.g. variable and values labels) from an `eatGADS`
object. This can be a `GADSdat`, an `all_GADSdat`, a labels
`data.frame`, or the path to an existing data base.

## Usage

``` r
extractMeta(GADSobject, vars = NULL)
```

## Arguments

- GADSobject:

  Either a `GADSdat` object or a path to an existing `eatGADS` data
  base.

- vars:

  A character vector containing variable names. If `NULL` (default), all
  available meta information is returned.

## Value

Returns a long format data frame with meta information.

## Details

Meta data is stored tidily in all `GADSdat` objects as a separate long
format data frame. This information can be extracted for a single or
multiple variables.

## Examples

``` r
# Extract Meta data from data base
db_path <- system.file("extdata", "pisa.db", package = "eatGADS")
extractMeta(db_path, vars = c("schtype", "sameteach"))
#>       varName                               varLabel format display_width
#> 358 sameteach Same math teacher in both school years   F8.0            NA
#> 359 sameteach Same math teacher in both school years   F8.0            NA
#> 360   schtype                           School track   F8.0            NA
#> 361   schtype                           School track   F8.0            NA
#> 362   schtype                           School track   F8.0            NA
#>     labeled value                                  valLabel missings data_table
#> 358     yes     1                                        No    valid      noImp
#> 359     yes     2                                       Yes    valid      noImp
#> 360     yes     1                Gymnasium (academic track)    valid      noImp
#> 361     yes     2                                Realschule    valid      noImp
#> 362     yes     3 schools with several courses of education    valid      noImp

# Extract Meta data from loaded/imported GADS
extractMeta(pisa, vars = c("schtype", "sameteach"))
#>     varName                               varLabel format display_width labeled
#> 5   schtype                           School track   F8.0            NA     yes
#> 6   schtype                           School track   F8.0            NA     yes
#> 7   schtype                           School track   F8.0            NA     yes
#> 8 sameteach Same math teacher in both school years   F8.0            NA     yes
#> 9 sameteach Same math teacher in both school years   F8.0            NA     yes
#>   value                                  valLabel missings
#> 5     1                Gymnasium (academic track)    valid
#> 6     2                                Realschule    valid
#> 7     3 schools with several courses of education    valid
#> 8     1                                        No    valid
#> 9     2                                       Yes    valid
```
