# Apply Meta Data Changes.

Function to apply meta data changes to a `GADSdat` object specified by a
change table extracted by
[`getChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/getChangeMeta.md).

## Usage

``` r
applyChangeMeta(changeTable, GADSdat, ...)

# S3 method for class 'varChanges'
applyChangeMeta(changeTable, GADSdat, checkVarNames = TRUE, ...)

# S3 method for class 'valChanges'
applyChangeMeta(
  changeTable,
  GADSdat,
  existingMeta = c("stop", "value", "value_new", "drop", "ignore"),
  ...
)
```

## Arguments

- changeTable:

  Change table as provided by
  [`getChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/getChangeMeta.md).

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- ...:

  further arguments passed to or from other methods.

- checkVarNames:

  Logical. Should new variable names be checked by
  [`checkVarNames`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)?

- existingMeta:

  If values are recoded, which meta data should be used (see details)?

## Value

Returns the modified `GADSdat` object.

## Details

Values for which the change columns contain `NA` remain unchanged. If
changes are performed on value levels, recoding into existing values can
occur. In these cases, `existingMeta` determines how the resulting meta
data conflicts are handled, either raising an error if any occur
(`"stop"`), keeping the original meta data for the value (`"value"`),
using the meta data in the `changeTable` and, if incomplete, from the
recoded value (`"value_new"`), or leaving the respective meta data
untouched (`"ignore"`).

Furthermore, one might recode multiple old values in the same new value.
This is currently only possible with `existingMeta = "drop"`, which
drops all related meta data on value level, or
`existingMeta = "ignore"`, which leaves all related meta data on value
level untouched.

## Examples

``` r
# Change a variable name and label
varChangeTable <- getChangeMeta(pisa, level = "variable")
varChangeTable[1, c("varName_new", "varLabel_new")] <- c("IDstud", "Person ID")

pisa2 <- applyChangeMeta(varChangeTable, GADSdat = pisa)
```
