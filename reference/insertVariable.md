# Reorder a single variable in a `GADSdat`.

Deprecated. Please use
[`relocateVariable`](https://beckerbenj.github.io/eatGADS/reference/relocateVariable.md)
instead.

## Usage

``` r
insertVariable(GADSdat, var, after = NULL)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- var:

  Character string of the variable name which should be sorted.

- after:

  Character string of the variable name after which `var` should be
  inserted. If `NULL`, `var` is inserted at the beginning of the
  `GADSdat`.
