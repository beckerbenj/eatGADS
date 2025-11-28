# Import R `data.frame`

Function to import a `data.frame` object for use in `eatGADS` while
extracting value labels from factors.

## Usage

``` r
import_DF(df, checkVarNames = TRUE)
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

Factors are integers with labeled variable levels. `import_DF` extracts
these labels and stores them in a separate meta data data.frame. See
[`import_spss`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
for detailed information.

## Examples

``` r
dat <- import_DF(iris, checkVarNames = FALSE)

# Inspect Meta data
extractMeta(dat)
#>        varName varLabel format display_width labeled value   valLabel missings
#> 1 Sepal.Length     <NA>   <NA>            NA      no    NA       <NA>     <NA>
#> 2  Sepal.Width     <NA>   <NA>            NA      no    NA       <NA>     <NA>
#> 3 Petal.Length     <NA>   <NA>            NA      no    NA       <NA>     <NA>
#> 4  Petal.Width     <NA>   <NA>            NA      no    NA       <NA>     <NA>
#> 5      Species     <NA>   <NA>            NA     yes     1     setosa    valid
#> 6      Species     <NA>   <NA>            NA     yes     2 versicolor    valid
#> 7      Species     <NA>   <NA>            NA     yes     3  virginica    valid

# Extract Data
dat <- extractData(dat, convertLabels = "character")
```
