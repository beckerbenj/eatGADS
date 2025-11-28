# Recode via lookup table.

Recode one or multiple variables based on a lookup table created via
[`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md)
(and potentially formatted by
[`collapseColumns`](https://beckerbenj.github.io/eatGADS/reference/collapseColumns.md)).

## Usage

``` r
applyLookup(GADSdat, lookup, suffix = NULL)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- lookup:

  Lookup table created by
  [`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md)
  and - if necessary - collapsed by
  [`collapseColumns`](https://beckerbenj.github.io/eatGADS/reference/collapseColumns.md).
  Column names must be `c("variable", "value", "value_new")`.

- suffix:

  Suffix to add to the existing variable names. If `NULL`, the old
  variables will be overwritten.

## Value

Returns a recoded `GADSdat`.

## Details

If there are missing values in the column `value_new`, `NAs` are
inserted as new values and a `warning` is issued.

The complete work flow when using a lookup table to recode multiple
variables in a `GADSdat` could be: (0) optional: Recode empty strings to
`NA` (necessary, if the look up table is written to excel). (1) create a
lookup table with
[`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md).
(2) Save the lookup table to `.xlsx` with `write_xlsx` from
`eatAnalysis`. (3) fill out the lookup table via `Excel`. (4) Import the
lookup table back to `R` via `read_excel` from `readxl`. (5) Apply the
final lookup table with `applyLookup`.

See
[`applyLookup_expandVar`](https://beckerbenj.github.io/eatGADS/reference/applyLookup_expandVar.md)
for recoding a single variable into multiple variables.

## Examples

``` r
## create an example GADSdat
iris2 <- iris
iris2$Species <- as.character(iris2$Species)
gads <- import_DF(iris2)
#> Sepal.Length has been renamed to Sepal_Length
#> Sepal.Width has been renamed to Sepal_Width
#> Petal.Length has been renamed to Petal_Length
#> Petal.Width has been renamed to Petal_Width

## create Lookup
lu <- createLookup(gads, recodeVars = "Species")
lu$value_new <- c("plant 1", "plant 2", "plant 3")

## apply lookup table
gads2 <- applyLookup(gads, lookup = lu, suffix = "_r")
#> No rows removed from meta data.
#> Adding meta data for the following variables: Species_r

## only recode some values
lu2 <- createLookup(gads, recodeVars = "Species")
lu2$value_new <- c("plant 1", "plant 2", NA)
gads3 <- applyLookup(gads, lookup = lu2, suffix = "_r")
#> Warning: Not all values have a recode value assigned (missings in value_new).
#> No rows removed from meta data.
#> Adding meta data for the following variables: Species_r
```
