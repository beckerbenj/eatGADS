# Fill imputed values.

Fill imputed values in a imputed `GADSdat_imp` object with original, not
imputed values from a `GADSdat`.

## Usage

``` r
fillImputations(GADSdat, GADSdat_imp, varName, varName_imp = varName, id, imp)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- GADSdat_imp:

  A `GADSdat` object.

- varName:

  A character vector of length 1 containing the variable name in
  `GADSdat`.

- varName_imp:

  A character vector of length 1 containing the variable name in
  `GADSdat_imp`.

- id:

  A character vector of length 1 containing the unique identifier column
  of both `GADSdat`.

- imp:

  A character vector of length 1 containing the imputation number in
  `GADSdat_imp`.

## Value

The modified `GADSdat_imp`..

## Details

This function only fills in missing values in the imputed variable from
the not imputed variable. It provides parts of the functionality of
`subImputations` but does not check whether values have been mistakenly
imputed. However, performance is increased substantially.

## Examples

``` r
# tbd
```
