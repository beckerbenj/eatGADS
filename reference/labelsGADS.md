# Labels from relational `eatGADS` data base.

Returns the variable and value labels of all variables in the `eatGADS`
data base.

## Usage

``` r
labelsGADS(filePath)
```

## Arguments

- filePath:

  Path of the existing `eatGADS` data base.

## Value

Returns a long format data frame including variable names, labels,
values, value labels and missing labels.

## Details

Variable, value and missing labels as stored in the original SPSS-files
and factors from R files are converted to long format for storage in the
data base. `labelsGADS` returns them as a long format data frame.

## Examples

``` r
# Extract Meta data from data base
db_path <- system.file("extdata", "pisa.db", package = "eatGADS")
metaData <- labelsGADS(db_path)
```
