# Import an object imported via `convertLabel`

Function to import a `data.frame` object created by `convertLabel` for
use in `eatGADS`. If possible, importing data via
[`import_spss`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
should always be preferred.

## Usage

``` r
import_convertLabel(df, checkVarNames = TRUE)
```

## Arguments

- df:

  A `data.frame`.

- checkVarNames:

  Should variable names be checked for violations of `SQLite` and `R`
  naming rules?

## Value

Returns a list with the actual data `dat` and a data frame with all meta
information in long format `labels`.

## Details

`convertLabel` from `R` package `eatAnalysis` converts an object
imported via `read.spss` (from the `foreign` package) to a `data.frame`
with factors and variable labels stored in variable attributes.
