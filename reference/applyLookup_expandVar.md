# Recode via lookup table into multiple variables.

Recode one or multiple variables based on a lookup table created via
[`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md).
In contrast to
[`applyLookup`](https://beckerbenj.github.io/eatGADS/reference/applyLookup.md),
this function allows the creation of multiple resulting variables from a
single input variable. All variables in `lookup` except `variable` and
`value` are treated as recode columns.

## Usage

``` r
applyLookup_expandVar(GADSdat, lookup)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- lookup:

  Lookup table created by
  [`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md).

## Value

Returns a recoded `GADSdat`.

## Details

If a variable contains information that should be split into multiple
variables via manual recoding, `applyLookup_expandVar` can be used. If
there are missing values in any recode column, `NAs` are inserted as new
values. A `warning` is issued only for the first column.

The complete work flow when using a lookup table to expand variables in
a `GADSdat` based on manual recoding could be: (1) create a lookup table
with
[`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md).
(2) Save the lookup table to `.xlsx` with `write_xlsx` from
`eatAnalysis`. (3) fill out the lookup table via `Excel`. (4) Import the
lookup table back to `R` via `read_excel` from `readxl`. (5) Apply the
final lookup table with `applyLookup_expandVar`.

See
[`applyLookup`](https://beckerbenj.github.io/eatGADS/reference/applyLookup.md)
for simply recoding variables in a `GADSdat`.

## Examples

``` r
## create an example GADSdat
example_df <- data.frame(ID = 1:6,
                        citizenship = c("germ", "engl", "germ, usa", "china",
                                        "austral, morocco", "nothin"),
                        stringsAsFactors = FALSE)
gads <- import_DF(example_df)

## create Lookup
lu <- createLookup(gads, recodeVars = "citizenship", addCol = c("cit_1", "cit_2"))
lu$cit_1 <- c("German", "English", "German", "Chinese", "Australian", NA)
lu$cit_2 <- c(NA, NA, "USA", NA, "Morocco", NA)

## apply lookup table
gads2 <- applyLookup_expandVar(gads, lookup = lu)
#> Warning: Not all values have a recode value assigned (missings in value_new).
#> No rows removed from meta data.
#> Adding meta data for the following variables: citizenship_1
#> No rows removed from meta data.
#> Adding meta data for the following variables: citizenship_2
```
