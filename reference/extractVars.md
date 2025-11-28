# Extract or remove variables from a `GADSdat`.

Extract or remove variables and their meta data from a `GADSdat` object.

## Usage

``` r
extractVars(GADSdat, vars)

removeVars(GADSdat, vars)
```

## Arguments

- GADSdat:

  `GADSdat` object.

- vars:

  A character vector containing the variables names in the `GADSdat`.

## Value

Returns a `GADSdat` object.

## Details

Both functions simply perform the variable removal or extraction from
the underlying `data.frame` in the `GADSdat` object followed by calling
[`updateMeta`](https://beckerbenj.github.io/eatGADS/reference/updateMeta.md).

## Examples

``` r
## create an example GADSdat
example_df <- data.frame(ID = 1:4,
                        age = c(12, 14, 16, 13),
                        citizenship1 = c("German", "English", "Polish", "Chinese"),
                        citizenship2 = c(NA, "German", "Chinese", "Polish"),
                        stringsAsFactors = TRUE)
gads <- import_DF(example_df)

## remove variables from GADSdat
gads2 <- removeVars(gads, vars = c("citizenship2", "age"))
#> Removing the following rows from meta data: age, citizenship2
#> No rows added to meta data.

## extract GADSdat with specific variables
gads3 <- extractVars(gads, vars = c("ID", "citizenship1"))
#> Removing the following rows from meta data: age, citizenship2
#> No rows added to meta data.
```
