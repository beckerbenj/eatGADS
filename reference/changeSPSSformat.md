# Change SPSS format.

Change the SPSS format of one or multiple variables as part of a
`GADSdat` object.

## Usage

``` r
changeSPSSformat(GADSdat, varName, format)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varName:

  Character vector of variable names.

- format:

  A single string containing the new SPSS format, for example 'A25' or
  'F10'.

## Value

Returns the `GADSdat` object with changed meta data..

## Details

Applied to a `GADSdat` or `all_GADSdat` object, this function is a
wrapper of
[`getChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/getChangeMeta.md)
and
[`applyChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md).

SPSS format is supplied following SPSS logic. `'A'` represents character
variables, `'F'` represents numeric variables. The number following this
letter represents the maximum width. Optionally, another number can be
added after a dot, representing the number of decimals in case of a
numeric variable. For instance, `'F8.2'` is used for a numeric variable
with a maximum width of 8 with 2 decimal numbers.

## Examples

``` r
# change SPSS format for a single variable (numeric variable with no decimals)
pisa2 <- changeSPSSformat(pisa, varName = "idstud",
                          format = "F10.0")

# change SPSS format for multiple variables (numeric variable with no decimals)
pisa2 <- changeSPSSformat(pisa, varName = c("idstud", "idschool"),
                          format = "F10.0")
```
