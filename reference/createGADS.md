# Create an `eatGADS` data base.

Creates a relational data base containing hierarchically stored data
with meta information (e.g. value and variable labels).

## Usage

``` r
createGADS(allList, pkList, fkList, filePath)
```

## Arguments

- allList:

  An object created via
  [`mergeLabels`](https://beckerbenj.github.io/eatGADS/reference/mergeLabels.md).

- pkList:

  List of primary keys.

- fkList:

  List of foreign keys.

- filePath:

  Path to the db file to write (including name); has to end on '.db'.

## Value

Creates a data base in the given path, returns `NULL`.

## Details

Uses [`createDB`](https://rdrr.io/pkg/eatDB/man/createDB.html) from the
`eatDB` package to create a relational data base. For details on how to
define keys see the documentation of
[`createDB`](https://rdrr.io/pkg/eatDB/man/createDB.html).

## Examples

``` r
# see createDB vignette
```
