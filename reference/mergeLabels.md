# Prepare data and metadata

Transform multiple `GADSdat` objects into a list ready for data base
creation.

## Usage

``` r
mergeLabels(...)
```

## Arguments

- ...:

  `GADSdat` objects, as named arguments in the correct merge order.

## Value

Returns an `all_GADSdat` object, which consists of list with a list of
all data frames `"datList"` and a single data frame containing all meta
data information `"allLabels"`.

## Details

The function
[`createGADS`](https://beckerbenj.github.io/eatGADS/reference/createGADS.md)
takes multiple `GADSdat` objects as input. The function preserves the
ordering in which the objects are supplied, which is then used for the
merging order in
[`createGADS`](https://beckerbenj.github.io/eatGADS/reference/createGADS.md).
Additionally, the separate lists of meta information for each `GADSdat`
are merged and a data frame unique identifier is added.

## Examples

``` r
# see createGADS vignette
```
