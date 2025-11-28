# Recode values to `NA`.

Recode multiple values in multiple variables in a `GADSdat` to `NA`.

## Usage

``` r
recode2NA(GADSdat, recodeVars = namesGADS(GADSdat), value = "")
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- recodeVars:

  Character vector of variable names which should be recoded.

- value:

  Which values should be recoded to `NA`?

## Value

Returns the recoded `GADSdat`.

## Details

If there are value labels given to the specified value, a warning is
issued. Number of recodes per variable are reported.

If a data set is imported from `.sav`, character variables frequently
contain empty strings. Especially if parts of the data are written to
`.xlsx`, this can cause problems (e.g. as lookup tables from
[`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md)),
as most function which write to `.xlsx` convert empty strings to `NAs`.
`recodeString2NA` can be used to recode all empty strings to `NA`
beforehand.

## Examples

``` r
# create example GADS
dat <- data.frame(ID = 1:4, var1 = c("", "Eng", "Aus", "Aus2"),
                  var2 = c("", "French", "Ger", "Ita"),
                  stringsAsFactors = FALSE)
gads <- import_DF(dat)

# recode empty strings
gads2 <- recode2NA(gads)
#> Recodes in variable ID: 0
#> Recodes in variable var1: 1
#> Recodes in variable var2: 1

# recode numeric value
gads3 <- recode2NA(gads, recodeVars = "ID", value = 1:3)
#> Recodes in variable ID: 3

```
