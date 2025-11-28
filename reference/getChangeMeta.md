# Extract table for Meta Data Changes.

Function to obtain a data frame from a `GADSdat` object for for changes
to meta data on variable or on value level.

## Usage

``` r
getChangeMeta(GADSdat, level = "variable")
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- level:

  `'variable'` or `'value'`.

## Value

Returns the meta data sheet for all variables including the
corresponding change columns.

## Details

Changes on variable level include variable names (`varName`), variable
labels (`varLabel`), SPSS format ((`format`)) and display width
(`display_width`). Changes on value level include values (`value`),
value labels (`valLabel`) and missing codes (`missings`).

## Examples

``` r
# For changes on variable level
varChangeTable <- getChangeMeta(pisa, level = "variable")

# For changes on value level
valChangeTable <- getChangeMeta(pisa, level = "value")
```
