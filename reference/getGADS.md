# Get data from GADS data base.

Extracts variables from a GADS data base. Only the specified variables
are extracted. Note that this selection determines the format of the
`data.frame` that is extracted.

## Usage

``` r
getGADS(vSelect = NULL, filePath)
```

## Arguments

- vSelect:

  Character vector of variable names.

- filePath:

  Path of the existing `eatGADS` data base file.

## Value

Returns a `GADSdat` object.

## Details

See [`createDB`](https://rdrr.io/pkg/eatDB/man/createDB.html) and
[`dbPull`](https://rdrr.io/pkg/eatDB/man/dbPull.html) for further
explanation of the query and merging processes.

## Examples

``` r
# Use data base within package
db_path <- system.file("extdata", "pisa.db", package = "eatGADS")
pisa_gads <- getGADS(db_path, vSelect = c("schtype", "sameteach"))
```
