# Checks compatibility of two `eatGADS` data bases.

This function checks if both data bases perform identical joins via
foreign keys, if they contain the same variable names and if these
variables have the same value labels. Results of this comparison are
reported on data table level as messages and as an output list.

## Usage

``` r
checkTrendStructure(filePath1, filePath2)
```

## Arguments

- filePath1:

  Path of the first `eatGADS` `.db` file.

- filePath2:

  Path of the second `eatGADS` `.db` file.

## Value

Returns a report list.

## Details

An error is thrown if the key structure or the data table structure
differs between the two data bases. Differences regarding meta data for
missing value labels and for variables labels (and formatting) are
ignored.

Reported differences regarding meta data can be inspected further via
[`inspectMetaDifferences`](https://beckerbenj.github.io/eatGADS/reference/inspectMetaDifferences.md).
