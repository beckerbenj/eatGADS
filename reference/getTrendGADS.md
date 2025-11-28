# Get data for trend reports.

Extracts variables from multiple `eatGADS` data bases. Data can then be
extracted from the `GADSdat` object via
[`extractData`](https://beckerbenj.github.io/eatGADS/reference/extractData.md).
For extracting meta data from a data base or a `GADSdat` object see
[`extractMeta`](https://beckerbenj.github.io/eatGADS/reference/extractMeta.md).
To speed up the data loading,
[`getGADS_fast`](https://beckerbenj.github.io/eatGADS/reference/getGADS_fast.md)
is used per default.

## Usage

``` r
getTrendGADS(
  filePaths,
  vSelect = NULL,
  years,
  fast = TRUE,
  tempPath = tempdir(),
  verbose = TRUE
)
```

## Arguments

- filePaths:

  Character vectors with paths to the `eatGADS` db files.

- vSelect:

  Variables from all GADS to be selected (as character vector).

- years:

  A numeric vector with identical length as `filePaths`.

- fast:

  Should
  [`getGADS_fast`](https://beckerbenj.github.io/eatGADS/reference/getGADS_fast.md)
  be used for data loading instead of
  [`getGADS`](https://beckerbenj.github.io/eatGADS/reference/getGADS.md)?
  Using the default is heavily recommended.

- tempPath:

  The directory, in which both GADS will be temporarily stored. Using
  the default is heavily recommended.

- verbose:

  Should the loading process be reported?

## Value

Returns a `GADSdat` object.

## Details

This function extracts data from multiple GADS data bases. All data
bases have to be created via
[`createGADS`](https://beckerbenj.github.io/eatGADS/reference/createGADS.md).
The data bases are joined via
[`rbind()`](https://rdrr.io/r/base/cbind.html) and a variable `year` is
added, corresponding to the argument `years`. The `GADSdat` object can
then further be used via
[`extractData`](https://beckerbenj.github.io/eatGADS/reference/extractData.md).
See [`createDB`](https://rdrr.io/pkg/eatDB/man/createDB.html) and
[`dbPull`](https://rdrr.io/pkg/eatDB/man/dbPull.html) for further
explanation of the querying and merging processes.

## Examples

``` r
# See getGADS vignette

```
