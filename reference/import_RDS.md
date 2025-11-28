# Import `RDS` file

Function to import a `data.frame` stored as a `.RDS` file while
extracting value labels from factors.

## Usage

``` r
import_RDS(filePath, checkVarNames = TRUE)
```

## Arguments

- filePath:

  Source file location, ending on `.RDS`.

- checkVarNames:

  Should variable names be checked for violations of `SQLite` and `R`
  naming rules?

## Value

Returns a list with the actual data `dat` and a data frame with all meta
information in long format `labels`.

## Details

Factors are integers with labeled variable levels. `import_RDS` extracts
these labels and stores them in a separate meta data data.frame. See
[`import_DF`](https://beckerbenj.github.io/eatGADS/reference/import_DF.md)
for detailed information. This function is a wrapper around
[`import_DF`](https://beckerbenj.github.io/eatGADS/reference/import_DF.md).
