# Extract Data 2

Extract `data.frame` from a `GADSdat` object for analyses in `R`. Per
default, missing codes are applied but value labels are dropped.
Alternatively, value labels can be selectively applied via
`labels2character`, `labels2factor`, and `labels2ordered`. For
extracting meta data see
[`extractMeta`](https://beckerbenj.github.io/eatGADS/reference/extractMeta.md).

## Usage

``` r
extractData2(
  GADSdat,
  convertMiss = TRUE,
  labels2character = NULL,
  labels2factor = NULL,
  labels2ordered = NULL,
  dropPartialLabels = TRUE
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- convertMiss:

  Should values tagged as missing values be recoded to `NA`?

- labels2character:

  For which variables should values be recoded to their labels? The
  resulting variables are of type `character`.

- labels2factor:

  For which variables should values be recoded to their labels? The
  resulting variables are of type `factor`.

- labels2ordered:

  For which variables should values be recoded to their labels? The
  resulting variables are of type `ordered`.

- dropPartialLabels:

  Should value labels for partially labeled variables be dropped? If
  `TRUE`, the partial labels will be dropped. If `FALSE`, the variable
  will be converted to the class specified in `labels2character`,
  `labels2factor`, or `labels2ordered`.

## Value

Returns a data frame.

## Details

A `GADSdat` object includes actual data (`GADSdat$dat`) and the
corresponding meta data information (`GADSdat$labels`). `extractData2`
extracts the data and applies relevant meta data on value level (missing
tags, value labels), so the data can be used for analyses in `R`.
Variable labels are retained as `label` attributes on column level.

If `factor` are extracted via `labels2factor` or `labels2ordered`, an
attempt is made to preserve the underlying integers. If this is not
possible, a warning is issued. As `SPSS` has almost no limitations
regarding the underlying values of labeled integers and `R`'s `factor`
format is very strict (no `0`, only integers increasing by `+ 1`), this
procedure can lead to frequent problems.

If multiple values of the same variable are assigned the same value
label and the variable should be transformed to `character`, `factor`,
or `ordered`, a warning is issued and the transformation is correctly
performed.

## Examples

``` r
# Extract Data for Analysis
dat <- extractData2(pisa)

# convert only some variables to character, all others remain numeric
dat <- extractData2(pisa, labels2character = c("schtype", "ganztag"))

# convert only some variables to factor, all others remain numeric
dat <- extractData2(pisa, labels2factor = c("schtype", "ganztag"))

# convert all labeled variables to factors
dat <- extractData2(pisa, labels2factor = namesGADS(pisa))

# convert somme variables to factor, some to character
dat <- extractData2(pisa, labels2character = c("schtype", "ganztag"),
                          labels2factor = c("migration"))
```
