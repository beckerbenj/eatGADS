# Import R data frame with explicit meta data sheets

Function to import a `data.frame` object for use in `eatGADS` while
adding explicit variable and value meta information through separate
`data.frames`.

## Usage

``` r
import_raw(df, varLabels, valLabels = NULL, checkVarNames = TRUE)
```

## Arguments

- df:

  A `data.frame`.

- varLabels:

  A `data.frame` containing the variable labels. All variables in the
  data have to have exactly one column in this data.frame.

- valLabels:

  A `data.frame` containing the value labels. All referenced variables
  have to appear in the data, but not all variables in the data have to
  receive value labels. Can be omitted.

- checkVarNames:

  Should variable names be checked for violations of `SQLite` and `R`
  naming rules?

## Value

Returns a list with the actual data `dat` and with all meta information
in long format `labels`.

## Details

The argument `varLables` has to contain exactly two variables, namely
`varName` and `varLabel`. `valLables` has to contain exactly four
variables, namely `varName`, `value`, `valLabel` and `missings`. The
column `value` can only contain numerical values. The column `missings`
can only contain the values `"valid"` and `"miss"`. Variables of type
`factor` are not supported in any of the `data.frames`.

## Examples

``` r
dat <- data.frame(ID = 1:5, grade = c(1, 1, 2, 3, 1))
varLabels <- data.frame(varName = c("ID", "grade"),
                       varLabel = c("Person Identifier", "School grade Math"),
                       stringsAsFactors = FALSE)
valLabels <- data.frame(varName = c("grade", "grade", "grade"),
                       value = c(1, 2, 3),
                       valLabel = c("very good", "good", "sufficient"),
                       missings = c("valid", "valid", "valid"),
                       stringsAsFactors = FALSE)

gads <- import_raw(df = dat, varLabels = varLabels, valLabels = valLabels, checkVarNames = FALSE)

# Inspect Meta data
extractMeta(gads)
#>   varName          varLabel format display_width labeled value   valLabel
#> 1      ID Person Identifier   <NA>            NA      no    NA       <NA>
#> 2   grade School grade Math   <NA>            NA     yes     1  very good
#> 3   grade School grade Math   <NA>            NA     yes     2       good
#> 4   grade School grade Math   <NA>            NA     yes     3 sufficient
#>   missings
#> 1     <NA>
#> 2    valid
#> 3    valid
#> 4    valid

# Extract Data
dat <- extractData(gads, convertLabels = "character")
```
