# Change variable labels.

Change variable labels of one or multiple variables as part of a
`GADSdat` object.

## Usage

``` r
changeVarLabels(GADSdat, varName, varLabel)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varName:

  Character vector of variable names.

- varLabel:

  Character vector of the new variable labels.

## Value

Returns the `GADSdat` object with changed meta data.

## Details

Applied to a `GADSdat` or `all_GADSdat` object, this function is a
wrapper of
[`getChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/getChangeMeta.md)
and
[`applyChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md).

## Examples

``` r
# Change one variable label
pisa2 <- changeVarLabels(pisa, varName = "repeated",
                         varLabel = c("Has a grade been repeated?"))

# Change multiple variable labels
pisa2 <- changeVarLabels(pisa, varName = c("repeated", "gender"),
                         varLabel = c("Has a grade been repeated?",
                                      "Student gender"))
```
