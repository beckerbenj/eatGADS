# Check and Adjust Missing Tags

Functions to check if missings are tagged and labeled correctly in a
`GADSdat` object.

## Usage

``` r
checkMissings(
  GADSdat,
  missingLabel = "missing",
  addMissingCode = TRUE,
  addMissingLabel = FALSE
)

checkMissingsByValues(GADSdat, missingValues = -50:-99, addMissingCode = TRUE)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- missingLabel:

  Single regular expression indicating how missing labels are commonly
  named in the value labels.

- addMissingCode:

  If `TRUE`, missing tags are added according to `missingLabel` or
  `missingValues`.

- addMissingLabel:

  If `TRUE`, `"generic missing"` is added according to occurrence of
  `"miss"` in `"missings"`. As often various value labels for missings
  are used, this argument should be used with great care.

- missingValues:

  Numeric vector of values which are commonly used for missing values.

## Value

Returns a `GADSdat` object with - if specified - modified missing tags.

## Details

`checkMissings()` compares value labels (`valLabels`) and missing tags
(`missings`) of a `GADSdat` object and its meta data information.
`checkMissingsByValues()` compares labeled values (`value`) and missing
tags (`missings`) of a `GADSdat` object and its meta data information.
Mismatches are reported and can be automatically adjusted. Note that all
checks are only applied to the meta data information, not the actual
data. For detecting missing value labels, see
[`checkMissingValLabels`](https://beckerbenj.github.io/eatGADS/reference/checkEmptyValLabels.md).

## Functions

- `checkMissings()`: compare missing tags and value labels

- `checkMissingsByValues()`: compare missing tags and values in a
  certain range

## Examples

``` r
# checkMissings
pisa2 <- changeValLabels(pisa, varName = "computer_age",
                        value = 5, valLabel = "missing: No computer use")

pisa3 <- checkMissings(pisa2)
#> The following variables have value labels including the term 'missing' which are not coded as missing:
#> computer_age
#> 'miss' is inserted into column missings for 1 rows.

# checkMissingsByValues
pisa4 <- changeValLabels(pisa, varName = "computer_age",
                        value = c(-49, -90, -99), valLabel = c("test1", "test2", "test3"))

pisa5 <- checkMissingsByValues(pisa4, missingValues = -50:-99)
#> The following variables have values in the 'missingValues' range which are not coded as missing:
#> computer_age
#> 'miss' is inserted into column missings for 2 rows.
```
