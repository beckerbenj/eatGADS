# Change value labels.

Change or add value labels of one or multiple variables as part of a
`GADSdat` object.

## Usage

``` r
changeValLabels(GADSdat, varName, value, valLabel)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varName:

  Character vector containing variable names.

- value:

  Numeric values which are being labeled.

- valLabel:

  Character vector of the new value labels. Labels are applied in the
  same ordering as `value`.

## Value

Returns the `GADSdat` object with changed meta data.

## Details

Applied to a `GADSdat` or `all_GADSdat` object, this function is a
wrapper of
[`getChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/getChangeMeta.md)
and
[`applyChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md).
The function supports changing multiple value labels (`valLabel`) as
well as value labels of multiple variables (`varName`) at once.

## Examples

``` r
# Change existing value labels
pisa2 <- changeValLabels(pisa, varName = "repeated",
                        value = c(1, 2),
                        valLabel = c("no grade repetition", "grade repitition"))

# Add value label to unlabeled value
mtcars_g <- import_DF(mtcars)
mtcars_g2 <- changeValLabels(mtcars_g, varName = "cyl",
                             value = c(4, 6, 8),
                             valLabel = c("four", "six", "eight"))

# Add value labels to multiple variables at once
mtcars_g3 <- changeValLabels(mtcars_g, varName = c("mpg", "cyl", "disp"),
                             value = c(-99, -98),
                             valLabel = c("missing", "not applicable"))

```
