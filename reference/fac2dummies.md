# Transform a factor variable to dummy variables.

Convert a factor variable with n levels to n dummy variables.

## Usage

``` r
fac2dummies(GADSdat, var)
```

## Arguments

- GADSdat:

  A `data.frame` or `GADSdat` object.

- var:

  A character vector with the name of the factor variable.

## Value

Returns a `GADSdat` containing the newly computed variables.

## Details

Newly created variables are named as the original variable with the
suffix `"_a"`, `"_b"` and so on. Variable labels are created by using
the original variable label (if available) and adding the value label of
the corresponding level. All missing codes are forwarded to all dummy
variables.

## Examples

``` r
## create an example GADSdat
suppressMessages(gads <- import_DF(iris))

## transform factor variable
gads2 <- fac2dummies(gads, var = "Species")
#> The following dummy variables have been created: Species_a, Species_b, Species_c

```
