# Split `GADSdat` into hierarchy levels.

Split a `GADSdat` into multiple, specified hierarchical levels.

## Usage

``` r
splitGADS(GADSdat, nameList)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- nameList:

  A list of character vectors. The names in the list correspond the the
  hierarchy levels.

## Value

Returns an `all_GADSdat` object, which consists of list with a list of
all data frames `"datList"` and a single data frame containing all meta
data information `"allLabels"`. For more details see also
[`mergeLabels`](https://beckerbenj.github.io/eatGADS/reference/mergeLabels.md).

## Details

The function takes a `GADSdat` object and splits it into its desired
hierarchical levels (a `all_GADSdat` object). Hierarchy level of a
variable is also accessible in the meta data via the column
`data_table`. If not all variable names are included in the `nameList`,
the missing variables will be dropped.

## Examples

``` r
# see createGADS vignette
```
