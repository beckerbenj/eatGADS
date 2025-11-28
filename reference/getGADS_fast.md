# Get data from GADS data base fast from server directory.

Extracts variables from a `eatGADS` data base. Only the specified
variables are extracted. Note that this selection determines the format
of the `data.frame` that is extracted. CAREFUL: This function uses a
local temporary directory to speed up loading the data base from a
server and caches the data base locally for a running R session. The
temporary data base is removed automatically when the running `R`
session is terminated.

## Usage

``` r
getGADS_fast(vSelect = NULL, filePath, tempPath = tempdir())
```

## Arguments

- vSelect:

  Character vector of variable names.

- filePath:

  Path of the existing `eatGADS` data base file.

- tempPath:

  Local directory in which the data base can temporarily be stored.
  Using the default is recommended.

## Value

Returns a `GADSdat` object.

## Details

A random temporary directory is used for caching the data base and is
removed, when the `R` sessions terminates. See
[`createDB`](https://rdrr.io/pkg/eatDB/man/createDB.html) and
[`dbPull`](https://rdrr.io/pkg/eatDB/man/dbPull.html) for further
explanation of the query and merging processes.
