# Remove special characters.

Remove special characters from a character vector or a `GADSdat` object.
Also suitable to fix encoding problems of a character vector or a
`GADSdat` object. See details for available options.

## Usage

``` r
fixEncoding(x, input = c("other", "ASCII", "windows1250", "BRISE"))
```

## Arguments

- x:

  A character vector or `GADSdat` object.

- input:

  Which encoding was used in
  [`import_spss`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md).

## Value

The modified character vector or `GADSdat` object.

## Details

The option `"other"` replaces correctly encoded special signs. The
option `"ASCII"` works for strings which were encoded presumably using
`UTF-8` and imported using `ASCII` encoding. The option `"windows1250"`
works for strings which were encoded presumably using `UTF-8` and
imported using `windows-1250` encoding. The option `"BRISE"` covers a
unique case used at the `FDZ at IQB`.

If entries are all upper case, special characters are also transformed
to all upper case (e.g., `"AE"` instead of `"Ae"`).

## Examples

``` r
fixEncoding(c("\U00C4pfel", "\U00C4PFEL", paste0("\U00DC", "ben"), paste0("\U00DC", "BEN")))
#> [1] "Aepfel" "AEPFEL" "Ueben"  "UEBEN" 
```
