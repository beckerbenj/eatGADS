# Extract single `GADSdat` from `all_GADSdat`

Function to extract a single `GADSdat` from an `all_GADSdat` object.

## Usage

``` r
extractGADSdat(all_GADSdat, name)
```

## Arguments

- all_GADSdat:

  `all_GADSdat` object

- name:

  A character vector with length 1 with the name of the `GADSdat`

## Value

Returns an `GADSdat` object.

## Details

`GADSdat` objects can be merged into a single `all_GADSdat` object via
[`mergeLabels`](https://beckerbenj.github.io/eatGADS/reference/mergeLabels.md).
This function, performs the reverse action, extracting a single
`GADSdat` object.

## Examples

``` r
# see createGADS vignette
```
