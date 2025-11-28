# Recode variables.

Recode one or multiple variables as part of a `GADSdat` or `all_GADSdat`
object.

## Usage

``` r
recodeGADS(
  GADSdat,
  varName,
  oldValues,
  newValues,
  existingMeta = c("stop", "value", "value_new", "drop", "ignore")
)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varName:

  Character vector containing variable names.

- oldValues:

  Vector containing the old values.

- newValues:

  Vector containing the new values (in the respective order as
  `oldValues`).

- existingMeta:

  If values are recoded, which meta data should be used (see details)?

## Value

Returns a `GADSdat`.

## Details

Applied to a `GADSdat` or `all_GADSdat` object, this function is a
wrapper of
[`getChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/getChangeMeta.md)
and
[`applyChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md).
Beyond that, unlabeled variables and values are recoded as well.
`oldValues` and `newValues` are matched by ordering in the function
call.

If changes are performed on value levels, recoding into existing values
can occur. In these cases, `existingMeta` determines how the resulting
meta data conflicts are handled, either raising an error if any occur
(`"stop"`), keeping the original meta data for the value (`"value"`),
using the meta data in the `changeTable` and, if incomplete, from the
recoded value (`"value_new"`), or leaving the respective meta data
untouched (`"ignore"`).

Furthermore, one might recode multiple old values in the same new value.
This is currently only possible with `existingMeta = "drop"`, which
drops all related meta data on value level, or
`existingMeta = "ignore"`, which leaves all related meta data on value
level untouched.

Missing values (`NA`) are supported in `oldValues` but not in
`newValues`. For recoding values to `NA` see
[`recode2NA`](https://beckerbenj.github.io/eatGADS/reference/recode2NA.md)
instead. For recoding character variables, using lookup tables via
[`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md)
is recommended. For changing value labels see
[`changeValLabels`](https://beckerbenj.github.io/eatGADS/reference/changeValLabels.md).

## Examples

``` r
# Example gads
example_df <- data.frame(ID = 1:5, color = c("blue", "blue", "green", "other", "other"),
                        animal = c("dog", "Dog", "cat", "hors", "horse"),
                        age = c(NA, 16, 15, 23, 50),
                        stringsAsFactors = FALSE)
example_df$animal <- as.factor(example_df$animal)
gads <- import_DF(example_df)

# simple recode
gads2 <- recodeGADS(gads, varName = "animal",
                   oldValues = c(3, 4), newValues = c(7, 8))
```
