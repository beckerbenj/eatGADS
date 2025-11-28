# Check names for `SQLite` column name conventions and length limits.

Checks names for `SQLite` column name conventions and `SPSS`/`Stata`
variable name limits, and applies appropriate variable name changes to
`GADSdat` or `all_GADSdat` objects.

## Usage

``` r
checkVarNames(
  GADSdat,
  checkKeywords = TRUE,
  checkDots = TRUE,
  checkDuplicates = TRUE,
  charLimits = NULL
)
```

## Arguments

- GADSdat:

  `GADSdat` or `all_GADSdat` object.

- checkKeywords:

  Logical. Should `SQLite` keywords be checked and modified?

- checkDots:

  Logical. Should occurrences of `"."` be checked and modified?

- checkDuplicates:

  Logical. Should case insensitive duplicate variable names be checked
  and modified?

- charLimits:

  Optional character vector of one or more program names(s) for the
  limit check (see details). Currently, these are implemented:
  `c("SPSS", "Stata")`

## Value

Returns the original object with updated variable names.

## Details

Invalid column names in a `SQLite` data base include

- `SQLite` keywords (see
  [`sqlite_keywords`](https://rdrr.io/pkg/eatDB/man/sqlite_keywords.html)),

- column names with a `"."` in it and

- duplicate variable names which arise from `SQLite` being case
  insensitive.

The corresponding variable name changes are

- appending the suffix `"Var"` to all `SQLite` keywords,

- changing all `"."` in variable names to `"_"`, and

- appending `"_2"` to case insensitive duplicated variable names.

Note that avoiding `"."` in variable names is beneficial for multiple
reasons, such as avoiding confusion with `S3` methods in `R` and issues
when exporting to `Stata`.

### Variable name length limits

The length of variable names is limited to 64 *bytes* in `SPSS` and to
32 *characters* in `Stata`. If more than one program name is provided in
`charLimits`, the most restrictive among the chosen limits will be
applied. Variable names exceeding that limit will be truncated and
marked with the suffix `"_tr"`.

## Examples

``` r
# Change example data set (create an invalid variable name)
pisa2 <- changeVarNames(pisa, oldNames = "computer_age",
                        newNames = "computer.age")
#> computer.age has been renamed to computer_age

pisa3 <- checkVarNames(pisa2)
```
