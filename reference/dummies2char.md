# Transform dummy variables to character variables.

Convert a set of dummy variables into a set of character variables.

## Usage

``` r
dummies2char(GADSdat, dummies, dummyValues, charNames, checkVarNames = TRUE)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- dummies:

  A character vector with the names of the dummy variables.

- dummyValues:

  A vector with the values which the dummy variables represent.

- charNames:

  A character vector containing the new variable names.

- checkVarNames:

  Logical. Should `charNames` be checked by
  [`checkVarNames`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)?

## Value

Returns a `GADSdat`.

## Details

A set of dummy variables is transformed to an equal number of character
variables. The character variables are aligned to the left and the
remaining character variables are set to `NA`. For each new variable the
missing codes of the respective dummy variable are reused.

## Examples

``` r
## create an example GADSdat
dummy_df <- data.frame(d1 = c("eng", "no eng", "eng"),
                      d2 = c("french", "french", "no french"),
                      d3 = c("no ger", "ger", "no ger"),
                      stringsAsFactors = TRUE)
dummy_g <- import_DF(dummy_df)

## transform dummy variables
dummy_g2 <- dummies2char(dummy_g, dummies = c("d1", "d2", "d3"),
                        dummyValues = c("english", "french", "german"),
                        charNames = c("char1", "char2", "char3"))

```
