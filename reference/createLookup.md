# Extract values for recoding.

Extract unique values from one or multiple variables of a `GADSdat`
object for recoding (e.g. via an Excel spreadsheet).

## Usage

``` r
createLookup(GADSdat, recodeVars, sort_by = NULL, addCols = c("value_new"))
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- recodeVars:

  Character vector of variable names which should be recoded.

- sort_by:

  By which column (`variable` and/or `value`) should the long format
  `data.frame` be sorted? If `NULL`, no sorting is performed.

- addCols:

  Character vector of additional column names for recoding purposes.

## Value

Returns a data frame in long format with the following variables:

- variable:

  Variables as specified in `recodeVars`

- value:

  Unique values of the variables specified in `recodeVars`

- value_new:

  This is the default for `addCols`. If different additional column
  names are supplied, this column is missing.

## Details

If recoding of one or multiple variables is more complex, a lookup table
can be created for later application via
[`applyLookup`](https://beckerbenj.github.io/eatGADS/reference/applyLookup.md)
or
[`applyLookup_expandVar`](https://beckerbenj.github.io/eatGADS/reference/applyLookup_expandVar.md).
The function allows the extraction of the values of multiple variables
and sorting of these unique values via `variable` and/or `values`. If
`addCols` are specified the lookup table has to be formatted via
[`collapseColumns`](https://beckerbenj.github.io/eatGADS/reference/collapseColumns.md),
before it can be applied to recode data.

## Examples

``` r
# create example GADS
dat <- data.frame(ID = 1:4, var1 = c(NA, "Eng", "Aus", "Aus2"),
                  var2 = c(NA, "French", "Ger", "Ita"),
                  stringsAsFactors = FALSE)
gads <- import_DF(dat)

# create Lookup table for recoding
lookup <- createLookup(gads, recodeVars = c("var1", "var2"), sort_by = c("value", "variable"))

# create Lookup table for recoding by multiple recoders
lookup2 <- createLookup(gads, recodeVars = c("var1", "var2"), sort_by = c("value", "variable"),
                        addCols = c("value_recoder1", "value_recoder2"))
```
