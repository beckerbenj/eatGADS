# Reorder a single variable in a `GADSdat`.

Reorder a single variable in a `GADSdat`. The variable (`var`) can be
inserted right after another variable (`after`) or at the beginning of
the `GADSdat` via `after = NULL`.

## Usage

``` r
relocateVariable(GADSdat, var, after = NULL)
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

## Value

Returns a `GADSdat` object.

## Details

The variables in the `dat` and in the `labels` section are ordered. For
reordering the whole `GADSdat`, see
[`orderLike`](https://beckerbenj.github.io/eatGADS/reference/orderLike.md).

## Examples

``` r
# Insert variable 'migration' after variable 'idclass'
pisa2 <- relocateVariable(pisa, var = "migration", after = "idclass")

# Insert variable 'idclass' at the beginning of the data set
pisa2 <- relocateVariable(pisa, var = "idclass", after = NULL)
```
