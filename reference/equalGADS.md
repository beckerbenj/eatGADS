# Test if two `GADSdat` objects are (nearly) equal

Run tests to check whether two `GADSdat` objects are (nearly) equal.
`equalData` compares variable names, number of rows in the data, and
data differences. `equalMeta` compares variable names and meta data
differences. `equalGADS` combines both functions. All functions produce
a test report in list format.

## Usage

``` r
equalGADS(
  target,
  current,
  id = NULL,
  metaExceptions = c("display_width", "labeled"),
  tolerance = sqrt(.Machine$double.eps)
)

equalData(target, current, id = NULL, tolerance = sqrt(.Machine$double.eps))

equalMeta(target, current, metaExceptions = c("display_width", "labeled"))
```

## Arguments

- target:

  A `GADSdat` object.

- current:

  A `GADSdat` object.

- id:

  A character vector containing the unique identifier columns of both
  `GADSdat`. If specified, both `GADSdat` are ordered according to `id`
  before comparing their data.

- metaExceptions:

  Should certain meta data columns be excluded from the comparison?

- tolerance:

  A numeric value greater than or equal to `0`. Differences smaller than
  `tolerance` are not reported. The default value is close to `1.5e-8`.

## Value

Returns a list with the following entries:

- names_not_in_1:

  Which variables are included in `current` but not in `target`?

- names_not_in_2:

  Which variables are included in `target` but not in `current`?

- data_nrow:

  Do the actual data sets have the same number of rows?

- data_differences:

  For which variables are the data different?

- meta_data_differences:

  For which variables are the meta data different?

## Details

More detailed checks for individual variables can be performed via
[`inspectDifferences`](https://beckerbenj.github.io/eatGADS/reference/inspectDifferences.md)
and
[`inspectMetaDifferences`](https://beckerbenj.github.io/eatGADS/reference/inspectMetaDifferences.md).
