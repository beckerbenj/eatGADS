# Create a variable.

Create an empty variable as part of a `GADSdat` object.

## Usage

``` r
createVariable(GADSdat, varName, checkVarName = TRUE)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varName:

  Name of the variable to be cloned.

- checkVarName:

  Logical. Should `varName` be checked by
  [`checkVarNames`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)?

## Value

Returns a `GADSdat`.

## Examples

``` r
# create a new variable
pisa_new <- createVariable(pisa, varName = "new_variable")
```
