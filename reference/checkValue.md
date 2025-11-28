# Check for a specific value

Function to look for occurrences of a specific value in a `GADSdat`.

## Usage

``` r
checkValue(GADSdat, value, vars = namesGADS(GADSdat))
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- value:

  Single string indicating how missing labels are commonly named in the
  value labels.

- vars:

  Character vector with the variable names to which `checkValue` should
  be applied.

## Value

A named integer.

## Details

The function checks occurrences of a specific value in a set of
variables (default: all variables) in the `GADSdat` and outputs a vector
containing the count of occurrences for all variables in which the value
occurs. It explicitly supports checking for `NA`.

## Examples

``` r
# for all variables in the data
checkValue(pisa, value = 99)
#>   idstud idschool 
#>        1        1 

# only for specific variables in the data
checkValue(pisa, vars = c("idschool", "g8g9"), value = 99)
#> idschool 
#>        1 
```
