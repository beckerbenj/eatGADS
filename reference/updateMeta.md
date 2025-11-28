# Update meta data.

Update the meta data of a `GADSdat` or `all_GADSdat` object according to
the variables in a new data object.

## Usage

``` r
updateMeta(GADSdat, newDat, checkVarNames = TRUE)
```

## Arguments

- GADSdat:

  `GADSdat` or `all_GADSdat` object.

- newDat:

  `data.frame` or list of `data.frames` with the modified data.
  `tibbles` and `data.tables` are currently not supported and need to be
  transformed to `data.frames` beforehand.

- checkVarNames:

  Logical. Should new variable names be checked by
  [`checkVarNames`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)?

## Value

Returns the original object with updated meta data (and removes factors
from the data).

## Details

If the data of a `GADSdat` or a `all_GADSdat` has changed (supplied via
`newDat`), `updateMeta` assimilates the corresponding meta data set. If
variables have been removed, the corresponding meta data is also
removed. If variables have been added, empty meta data is added for
these variables. Factors are transformed to numerical and their levels
added to the meta data set.

## Examples

``` r
# see createGADS vignette
```
