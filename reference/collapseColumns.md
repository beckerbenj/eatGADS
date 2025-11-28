# Collapse two columns of a lookup table.

Collapse two columns or format a single column of a lookup table created
by
[`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md).

## Usage

``` r
collapseColumns(lookup, recodeVars, prioritize)
```

## Arguments

- lookup:

  For example a lookup table `data.frame` as created via
  [`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md).

- recodeVars:

  Character vector of column names which should be collapsed (currently
  only up to two variables are supported).

- prioritize:

  Character vector of length 1. Which of the columns in `recodeVars`
  should be prioritized, if multiple values are available? If
  `recodeVars` is of length 1, this argument can be omitted.

## Value

Returns a `data.frame` that can be used for
[`applyLookup`](https://beckerbenj.github.io/eatGADS/reference/applyLookup.md),
with the columns:

- variable:

  Variable names

- value:

  Old values

- value_new:

  New values. Renamed and/or collapsed column.

## Details

If a lookup table is created by
[`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md),
different recoding columns can be specified by the `addCols` argument.
This might be the case if two rater suggest recodes or one rater
corrects recodes by another rater in a separate column. After the
recoding columns have been filled out, `collapseColumns` can be used to
either:

\(a\) Collapse two recoding columns into one recoding column. This might
be desirable, if the two columns contain missing values. `prioritize`
can be used to specify, which of the two columns should be prioritized
if both columns contain valid values.

\(b\) Format the lookup table for
[`applyLookup`](https://beckerbenj.github.io/eatGADS/reference/applyLookup.md),
if `recodeVars` is a single variable. This simply renames the single
variable specified under `recodeVars`.

## Examples

``` r
## (a) Collapse two columns
# create example recode data.frame
lookup_raw <- data.frame(variable = c("var1"), value = c("germa", "German", "dscherman"),
           recode1 = c(NA, "English", "German"),
           recode2 = c("German", "German", NA), stringsAsFactors = FALSE)

# collapse columns
lookup <- collapseColumns(lookup_raw, recodeVars = c("recode1", "recode2"), prioritize = "recode2")

## (b) Format one column
# create example recode data.frame
lookup_raw2 <- data.frame(variable = c("var1"), value = c("germa", "German", "dscherman"),
           recode1 = c("German", "German", "German"), stringsAsFactors = FALSE)

# collapse columns
lookup2 <- collapseColumns(lookup_raw2, recodeVars = c("recode1"))
```
