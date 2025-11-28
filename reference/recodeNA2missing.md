# Recode `NAs` to Missing.

Recode `NAs` in multiple variables in a `GADSdat` to a numeric value
with a value label and a missing tag.

## Usage

``` r
recodeNA2missing(
  GADSdat,
  recodeVars = namesGADS(GADSdat),
  value = -99,
  valLabel = "missing"
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- recodeVars:

  Character vector of variable names which should be recoded.

- value:

  Which value should `NAs` be recoded to?

- valLabel:

  Which value label should `value` be assigned?

## Value

Returns the recoded `GADSdat`.

## Details

The value label and missing tag are only added to variables which
contain `NAs` and which have been recoded. If a variable has an existing
value label for `value`, the existing value label is overwritten and a
missing tag is added. A corresponding warning is issued.

## Examples

``` r
# create example GADS
dat <- data.frame(ID = 1:4, age = c(NA, 18, 21, 23),
                  siblings = c(0, 2, NA, NA))
gads <- import_DF(dat)

# recode NAs
gads2 <- recodeNA2missing(gads)
```
