# Calculate a scale.

Calculate a scale variable based on multiple items.

## Usage

``` r
calculateScale(
  GADSdat,
  items,
  scale,
  maxNA = length(items),
  reportDescr = FALSE
)
```

## Arguments

- GADSdat:

  A `data.frame` or `GADSdat` object.

- items:

  A character vector with all item variable names.

- scale:

  A character vector with the scale name.

- maxNA:

  Maximum number of allowed `NA` values on the items.

- reportDescr:

  Should descriptive statistics be reported for the calculated scale.

## Value

Returns a `GADSdat` containing the newly computed variable.

## Details

Descriptive statistics (including Cronbach's alpha, credit to the `psy`
package) are calculated and printed to the console. The new scale
variable is automatically inserted right after the last item in the
original `GADSdat`.

## Examples

``` r
##
items <- paste0("norms_", letters[1:6])
pisa_new <- calculateScale(pisa, items = items, scale = "norms")
```
