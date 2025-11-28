# Recode a string to `NA`.

Deprecated, use
[`recode2NA`](https://beckerbenj.github.io/eatGADS/reference/recode2NA.md)
instead..

## Usage

``` r
recodeString2NA(GADSdat, recodeVars = namesGADS(GADSdat), string = "")
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- recodeVars:

  Character vector of variable names which should be recoded.

- string:

  Which string should be recoded to `NA`?

## Value

Returns the recoded `GADSdat`.
