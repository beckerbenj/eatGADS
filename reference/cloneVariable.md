# Clone a variable.

Clone a variable as part of a `GADSdat` object.

## Usage

``` r
cloneVariable(
  GADSdat,
  varName,
  new_varName,
  label_suffix = "",
  checkVarName = TRUE
)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varName:

  Name of the variable to be cloned.

- new_varName:

  Name of the new variable.

- label_suffix:

  Suffix added to variable label for the newly created variable in the
  `GADSdat`.

- checkVarName:

  Logical. Should `new_varName` be checked by
  [`checkVarNames`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)?

## Value

Returns a `GADSdat`.

## Details

The variable is simply duplicated and assigned a new name.

## Examples

``` r
# duplicate the variable schtype
pisa_new <- cloneVariable(pisa, varName = "schtype", new_varName = "schtype_new")
```
