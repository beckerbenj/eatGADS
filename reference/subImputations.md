# Substitute imputed values.

Substitute imputed values in a imputed `GADSdat_imp` object with
original, not imputed values from a `GADSdat`.

## Usage

``` r
subImputations(GADSdat, GADSdat_imp, varName, varName_imp = varName, id, imp)
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

There are two cases in which values are substituted: (a) there are
missings in `varName_imp`, (b) values have been imputed even though
there is valid information in `varName`.

## Examples

``` r
# tbd
```
