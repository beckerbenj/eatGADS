# Create data.frame for specification of numerical plausibility checks.

All numerical variables without value labels in a `GADSdat` are selected
and a `data.frame` is created, which allows the specification of minima
and maxima.

## Usage

``` r
createNumCheck(GADSdat)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

## Value

A data.frame with the following variables:

- variable:

  All numerical variables in the `GADSdat`

- varLabel:

  Corresponding variable labels

- min:

  Minimum value for the specific variable.

- max:

  Maximum value for the specific variable.

- value_new:

  Which value should be inserted if values exceed the specified range?

## Details

This function is currently under development.

## Examples

``` r
# tbd
```
