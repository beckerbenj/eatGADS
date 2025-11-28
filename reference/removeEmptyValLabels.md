# Remove unused value labels and missing tags.

Remove unused value labels and missing tags of a variable as part of a
`GADSdat` object.

## Usage

``` r
removeEmptyValLabels(GADSdat, vars, whichValLabels = c("miss", "valid", "all"))
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- vars:

  Character string of variable names.

- whichValLabels:

  Should unused missing value tags and labels (`"miss"`), unused value
  labels for non-missing values (`"valid"`), or both (`"all"`) be
  removed?

## Value

Returns the `GADSdat` object with changed meta data.

## Examples

``` r
gads <- import_DF(data.frame(v1 = 1))
gads <- changeMissings(gads, varName = "v1", value = c(-99, -98), missings = c("miss", "miss"))
gads <- changeValLabels(gads, varName = "v1", value = c(-99), valLabel = c("not reached"))

gads2 <- removeEmptyValLabels(gads, vars = "v1")
```
