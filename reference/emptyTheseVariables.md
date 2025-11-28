# Set variables to `NA`.

Set all values within one or multiple variables to `NA`.

## Usage

``` r
emptyTheseVariables(GADSdat, vars, label_suffix = "")
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- vars:

  Character vector of variable names which should be set to `NA`.

- label_suffix:

  Suffix added to variable labels for the affected variables in the
  `GADSdat`.

## Value

Returns the recoded `GADSdat`.

## Examples

``` r
# empty multiple variables
pisa2 <- emptyTheseVariables(pisa, vars = c("idstud", "idschool"))
```
