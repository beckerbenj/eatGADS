# Extract Data while merging linking errors.

Support for linking error data bases has been removed from `eatGADS`.
`extractDataOld` provides (for the time being) backwards compatibility,
so linking errors can still be merged automatically.

## Usage

``` r
extractDataOld(
  GADSdat,
  convertMiss = TRUE,
  convertLabels = "character",
  dropPartialLabels = TRUE,
  convertVariables = NULL
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- convertMiss:

  Should values coded as missing values be recoded to `NA`?

- convertLabels:

  If `"numeric"`, values remain as numerics. If `"factor"` or
  `"character"`, values are recoded to their labels. Corresponding
  variable type is applied.

- dropPartialLabels:

  Should value labels for partially labeled variables be dropped? If
  `TRUE`, the partial labels will be dropped. If `FALSE`, the variable
  will be converted to the class specified in `convertLabels`.

- convertVariables:

  Character vector of variables names, which labels should be applied
  to. If not specified (default), value labels are applied to all
  variables for which labels are available. Variable names not in the
  actual GADS are silently dropped.

## Value

Returns a data frame.

## Details

See
[`extractData`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)
for the current functionality.
