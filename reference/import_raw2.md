# Import R data frame with a explicit meta data sheet

Function to create a `GADSdat` object based on a `dat` `data.frame` and
a `labels` `data.frame`.

## Usage

``` r
import_raw2(dat, labels)
```

## Arguments

- dat:

  A `dat` `data.frame` containing all actual data.

- labels:

  A `labels` `data.frame` containing all meta data.

## Value

Returns a `GADSdat` object.

## Details

A `GADSdat` is basically a `list` with two elements: a `dat` and a
`labels` `data.frame`. If these elements are separated, they can be
cleanly tied together again by `import_raw2`. The function performs
extensive checks on the integrity of the resulting `GADSdat` object. See
[`import_spss`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
and
[`import_raw`](https://beckerbenj.github.io/eatGADS/reference/import_raw.md)
for further details.

## Examples

``` r
dat <- data.frame(ID = 1:5, grade = c(1, 1, 2, 3, 1))
varLabels <- data.frame(varName = c("ID", "grade"),
                       varLabel = c("Person Identifier", "School grade Math"),
                       stringsAsFactors = FALSE)
valLabels <- data.frame(varName = c("grade", "grade", "grade"),
                       value = c(1, 2, 3),
                       valLabel = c("very good", "good", "sufficient"),
                       missings = c("valid", "valid", "valid"),
                       stringsAsFactors = FALSE)

gads <- import_raw(df = dat, varLabels = varLabels, valLabels = valLabels, checkVarNames = FALSE)

# separate the GADSdat object
dat <- gads$dat
labels <- gads$labels

# rejoin it
dat <- import_raw2(dat, labels)
```
