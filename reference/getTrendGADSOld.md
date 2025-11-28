# Get data for trend reports.

Support for linking error data bases has been removed from `eatGADS`.
`getGADSold` provides (for the time being) backwards compatibility, so
linking errors can still be extracted automatically.

## Usage

``` r
getTrendGADSOld(
  filePath1,
  filePath2,
  lePath = NULL,
  vSelect = NULL,
  years,
  fast = TRUE,
  tempPath = tempdir()
)
```

## Arguments

- filePath1:

  Path of the first `eatGADS` db file.

- filePath2:

  Path of the second `eatGADS` db file.

- lePath:

  Path of the linking error db file. If `NULL`, no linking errors are
  added to the data.

- vSelect:

  Variables from both GADS to be selected (as character vector).

- years:

  A numeric vector of length 2. The first elements corresponds to
  `filePath1`, the second element to `filePath2`.

- fast:

  Should
  [`getGADS_fast`](https://beckerbenj.github.io/eatGADS/reference/getGADS_fast.md)
  be used for data loading instead of
  [`getGADS`](https://beckerbenj.github.io/eatGADS/reference/getGADS.md)?
  Using the default is heavily recommended.

- tempPath:

  The directory, in which both GADS will be temporarily stored. Using
  the default is heavily recommended.

## Value

Returns a `GADSdat` object.

## Details

See
[`getGADS`](https://beckerbenj.github.io/eatGADS/reference/getGADS.md)
for the current functionality.

## Examples

``` r
# See getGADS vignette

```
