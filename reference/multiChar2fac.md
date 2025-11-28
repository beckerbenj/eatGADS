# Transform one or multiple character variables to factor.

Convert one or multiple character variables to factors. If multiple
variables are converted, a common set of value labels is created, which
is identical across variables. Existing value labels are preserved.

## Usage

``` r
multiChar2fac(
  GADSdat,
  vars,
  var_suffix = "_r",
  label_suffix = "(recoded)",
  convertCases = NULL
)
```

## Arguments

- GADSdat:

  A `data.frame` or `GADSdat` object.

- vars:

  A character vector with all variables that should be transformed to
  factor.

- var_suffix:

  Variable suffix for the newly created `GADSdat`. If an empty
  character, the existing variables are overwritten.

- label_suffix:

  Suffix added to variable label for the newly created variable in the
  `GADSdat`.

- convertCases:

  Should cases be transformed for all variables? Default `NULL` leaves
  cases as they are. Available options for converting cases are all
  lower case (`'lower'`), all upper case (`'upper'`), or first letter
  upper case, everything else lower case (`'upperFirst'`).

## Value

Returns a `GADSdat` containing the newly computed variable.

## Details

If a set of variables has the same possible values, it is desirable that
these variables share the same value labels, even if some of the values
do not occur on the individual variables. This function allows the
transformation of multiple character variables to factors while
assimilating the value labels. The SPSS format of the newly created
variables is set to `F10.0`.

A current limitation of the function is that prior to the conversion,
all variables specified in `vars` must have identical meta data on value
level (value labels and missing tags).

If necessary, missing codes can be set after transformation via
[`checkMissings`](https://beckerbenj.github.io/eatGADS/reference/checkMissings.md)
for setting missing codes depending on value labels for all variables or
[`changeMissings`](https://beckerbenj.github.io/eatGADS/reference/changeMissings.md)
for setting missing codes for specific values in a specific variable.

The argument `convertCases` uses the function
[`convertCase`](https://beckerbenj.github.io/eatGADS/reference/convertCase.md)
internally. See the respective documentation for more details.

## Examples

``` r
## create an example GADSdat
example_df <- data.frame(ID = 1:4,
                        citizenship1 = c("German", "English", "missing by design", "Chinese"),
                        citizenship2 = c("missing", "German", "missing by design", "Polish"),
                        stringsAsFactors = FALSE)
gads <- import_DF(example_df)

## transform one character variable
gads2 <- multiChar2fac(gads, vars = "citizenship1")

## transform multiple character variables
gads2 <- multiChar2fac(gads, vars = c("citizenship1", "citizenship2"))

## set values to missings
gads3 <- checkMissings(gads2, missingLabel = "missing")
#> The following variables have value labels including the term 'missing' which are not coded as missing:
#> citizenship1_r, citizenship2_r
#> 'miss' is inserted into column missings for 4 rows.
```
