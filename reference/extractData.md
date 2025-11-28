# Extract Data

Extract `data.frame` from a `GADSdat` object for analyses in `R`. Value
labels can be selectively applied via defining `convertLabels` and
`covertVariables`. For extracting meta data see
[`extractMeta`](https://beckerbenj.github.io/eatGADS/reference/extractMeta.md).

## Usage

``` r
extractData(
  GADSdat,
  convertMiss = TRUE,
  convertLabels = c("character", "factor", "numeric"),
  convertVariables = NULL,
  dropPartialLabels = TRUE
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- convertMiss:

  Should values tagged as missing values be recoded to `NA`?

- convertLabels:

  If `"numeric"`, values remain as numerics. If `"factor"` or
  `"character"`, values are recoded to their labels. Corresponding
  variable type is applied.

- convertVariables:

  Character vector of variables names, which labels should be applied
  to. All other variables remain as numeric variables in the data. If
  not specified \[default\], value labels are applied to all variables
  for which labels are available. Variable names not in the actual
  `GADS` are silently dropped.

- dropPartialLabels:

  Should value labels for partially labeled variables be dropped? If
  `TRUE`, the partial labels will be dropped. If `FALSE`, the variable
  will be converted to the class specified in `convertLabels`.

## Value

Returns a data frame.

## Details

A `GADSdat` object includes actual data (`GADSdat$dat`) and the
corresponding meta data information (`GADSdat$labels`). `extractData`
extracts the data and applies relevant meta data on value level (missing
conversion, value labels), so the data can be used for analyses in `R`.
Variable labels are retained as `label` attributes on column level.

If `factor` are extracted via `convertLabels == "factor"`, an attempt is
made to preserve the underlying integers. If this is not possible, a
warning is issued. As `SPSS` has almost no limitations regarding the
underlying values of labeled integers and `R`'s `factor` format is very
strict (no `0`, only integers increasing by `+ 1`), this procedure can
lead to frequent problems.

## Examples

``` r
# Extract Data for Analysis
dat <- extractData(pisa)

# convert labeled variables to factors
dat <- extractData(pisa, convertLabels = "factor")

# convert only some variables to factor, all others remain numeric
dat <- extractData(pisa, convertLabels = "factor", convertVariables = c("schtype", "ganztag"))

# convert only some variables to character, all others remain numeric
dat <- extractData(pisa, convertLabels = "factor", convertVariables = c("schtype", "ganztag"))
# schtype is now character
table(dat$schtype)
#> 
#>                Gymnasium (academic track) 
#>                                       221 
#>                                Realschule 
#>                                       168 
#> schools with several courses of education 
#>                                       111 
# schtype remains numeric
table(dat$gender)
#> 
#>   1   2 
#> 257 243 
```
