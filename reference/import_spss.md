# Import SPSS data

Function to import `.sav` files while extracting meta information, e.g.
variable and value labels.

## Usage

``` r
import_spss(
  filePath,
  checkVarNames = TRUE,
  labeledStrings = c("drop", "keep", "transform"),
  encoding = NULL
)
```

## Arguments

- filePath:

  Source file location, ending on `.sav`.

- checkVarNames:

  Should variable names be checked for violations of `SQLite` and `R`
  naming rules?

- labeledStrings:

  Should strings as labeled values be allowed? If `"drop"` (default),
  all labeled strings are dropped and `NAs` occur in the meta data. If
  `"transform"`, all underlying values are transformed to numeric. If
  `"keep"`, value labels stay untouched. However, the latter possibly
  corrupts all labeled values.

- encoding:

  The character encoding used for the file. The default, `NULL`, use the
  encoding specified in the file, but sometimes this value is incorrect
  and it is useful to be able to override it.

## Value

Returns a list with the actual data `dat` and a data frame with all meta
information in long format `labels`.

## Details

SPSS files (`.sav`) store variable and value labels and assign specific
formatting to variables. `import_spss` imports data from SPSS, while
storing this meta-information separately in a long format data frame.
Value labels and missing labels are used to identify missing values (see
[`checkMissings`](https://beckerbenj.github.io/eatGADS/reference/checkMissings.md)).
Time and date variables are converted to character.

In some special cases, `.sav` files seem to consist of a mix of
different encoding types. In such cases, `haven` might throw an error if
the encoding argument is not specified or `UTF-8` is selected as
encoding. To circumvent this problem we recommend using
`encoding = "ASCII"` and fixing the resulting issues manually. For
example,
[`fixEncoding`](https://beckerbenj.github.io/eatGADS/reference/fixEncoding.md)
provides some fixes for encoding issues specific to the German language.

## Examples

``` r
# Use spss data from within package
spss_path <- system.file("extdata", "pisa.zsav", package = "eatGADS")
pisa_gads <- import_spss(spss_path)
```
