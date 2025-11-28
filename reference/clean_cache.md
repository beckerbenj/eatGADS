# Clean temporary cache.

Deprecated. The cached data base is now cleaned when the R sessions ends
automatically.

## Usage

``` r
clean_cache(tempPath = tempdir())
```

## Arguments

- tempPath:

  Local directory in which the data base was temporarily be stored.

## Value

Returns nothing.

## Details

Cleans the temporary cache, specified by
[`tempdir()`](https://rdrr.io/r/base/tempfile.html). This function had
to be executed at the end of an `R` session if
[`getGADS_fast`](https://beckerbenj.github.io/eatGADS/reference/getGADS_fast.md)
or
[`getTrendGADS`](https://beckerbenj.github.io/eatGADS/reference/getTrendGADS.md)
with `fast = TRUE` had been used.
