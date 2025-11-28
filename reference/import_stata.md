# Import `Stata` data

Function to import `.dta` files while extracting meta information, e.g.
variable and value labels.

## Usage

``` r
import_stata(filePath, checkVarNames = TRUE, labeledStrings = FALSE)
```

## Arguments

- filePath:

  Source file location, ending on `.dta`.

- checkVarNames:

  Should variable names be checked for violations of `SQLite` and `R`
  naming rules?

- labeledStrings:

  Should strings as labeled values be allowed? This possibly corrupts
  all labeled values.

## Value

Returns a list with the actual data `dat` and a data frame with all meta
information in long format `labels`.

## Details

`Stata` files (`.dta`) store variable and value labels and assign
specific formatting to variables. `import_stata` imports data from
`Stata`, while storing this meta-information separately in a long format
data frame. Time and date variables are converted to character.
