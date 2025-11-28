# Use meta data for variables from another `GADSdat`.

Transfer meta information from one `GADSdat` to another for one or
multiple variables.

## Usage

``` r
reuseMeta(
  GADSdat,
  varName,
  other_GADSdat,
  other_varName = NULL,
  missingLabels = NULL,
  addValueLabels = FALSE
)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varName:

  Character vector with the names of the variables that should get the
  new meta data.

- other_GADSdat:

  `GADSdat` object imported via `eatGADS` including the desired meta
  information. Can either be a `GADSdat`, an `eatGADS` data base or an
  `all_GADSdat` object.

- other_varName:

  Character vector with the names of the variables in `other_GADSdat`
  that contain the meta data which should be copied.

- missingLabels:

  How should meta data for missing values be treated? If `NULL`, missing
  values are transferred as all other labels. If `"drop"`, missing
  labels are dropped (useful for imputed data). If `"leave"`, missing
  labels remain untouched. If `"only"`, all valid value labels are
  dropped.

- addValueLabels:

  Should only value labels be added and all other meta information
  retained?

## Value

Returns the original object with updated meta data.

## Details

Transfer of meta information can mean substituting the complete meta
information, only adding value labels, adding only `"valid"` or adding
only `"miss"` missing labels. See the arguments `missingLabels` and
`addValueLabels` for further details.

## Examples

``` r
# see createGADS vignette
```
