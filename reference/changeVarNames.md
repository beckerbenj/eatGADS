# Change Variable Names.

Change variable names of a `GADSdat` or `all_GADSdat` object.

## Usage

``` r
changeVarNames(GADSdat, oldNames, newNames, checkVarNames = TRUE)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- oldNames:

  Vector containing the old variable names.

- newNames:

  Vector containing the new variable names, in identical order as
  `oldNames`.

- checkVarNames:

  Logical. Should new variable names be checked by
  [`checkVarNames`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)?

## Value

Returns the `GADSdat` object with changed variable names.

## Details

Applied to a `GADSdat` or `all_GADSdat` object, this function is a
wrapper of
[`getChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/getChangeMeta.md)
and
[`applyChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)

## Examples

``` r
# Change multiple variable name
pisa2 <- changeVarNames(pisa, oldNames = c("idstud", "idschool"),
                        newNames = c("IDstud", "IDschool"))
```
