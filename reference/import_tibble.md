# Import `tibble`

Function to import a `tibble` while extracting meta information, e.g.
variable and value labels.

## Usage

``` r
import_tibble(
  tibble,
  checkVarNames = TRUE,
  labeledStrings = c("drop", "keep", "transform")
)
```

## Arguments

- tibble:

  A `tibble` object.

- checkVarNames:

  Should variable names be checked for violations of `SQLite` and `R`
  naming rules?

- labeledStrings:

  Should strings as labeled values be allowed? If `"drop"` (default),
  all labeled strings are dropped and `NAs` occur in the meta data. If
  `"transform"`, all underlying values are transformed to numeric. If
  `"keep"`, value labels stay untouched. However, the latter possibly
  corrupts all labeled values.

## Value

Returns a list with the actual data `dat` and a data frame with all meta
information in long format `labels`.

## Details

`Tibbles` may store variable and value labels as well as missing tags
via the `labelled` class. `import_tibble` restructures this meta
information separately in a long format `data.frame`. Value labels and
missing tags are used to identify missing tags (see
[`checkMissings`](https://beckerbenj.github.io/eatGADS/reference/checkMissings.md)).
Time and date variables are converted to character.

## Examples

``` r
# Use spss data from within package
spss_path <- system.file("extdata", "pisa.zsav", package = "eatGADS")
pisa_gads <- import_spss(spss_path)
```
