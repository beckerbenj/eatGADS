# Match regular expressions and variable names.

Using variable labels, `matchValues_varLabels` matches a vector of
regular expressions to a set of variable names.

## Usage

``` r
matchValues_varLabels(GADSdat, mc_vars, values, label_by_hand = character(0))
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- mc_vars:

  A vector containing the names of the variables, which should be
  matched according to their variable labels.

- values:

  A character vector containing the regular expressions for which the
  `varLabel` column should be searched.

- label_by_hand:

  Additional value - `mc_var` pairs. Necessary, if for some `mc_vars` no
  value exists.

## Value

Returns a named character vector. Values of the vector are the variable
names in the `GADSdat`, names of the vector are the regular expressions.

## Details

Multiple choice items can be stored as multiple dichotomous variables
with the information about the variable stored in the variable labels.
The function
[`collapseMultiMC_Text`](https://beckerbenj.github.io/eatGADS/reference/collapseMultiMC_Text.md)
can be used to collapse such dichotomous variables and a character
variable, but requires a character vector with variables names of the
multiple choice variables. `matchValues_varLabels` creates such a vector
based on matching regular expressions (`values`) to variable labels.

Note that all variables in `mc_vars` have to be assigned exactly one
value (and vice versa). If a variable name is missing in the output, an
error will be thrown. In this case, the `label_by_hand` argument should
be used to specify the regular expression variable name pair manually.

## Examples

``` r
# Prepare example data
mt2 <- data.frame(ID = 1:4, mc1 = c(1, 0, 0, 0), mc2 = c(0, 0, 0, 0), mc3 = c(0, 1, 1, 0),
                  text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, NA),
                  stringsAsFactors = FALSE)

mt2_gads <- import_DF(mt2)

mt3_gads <- changeVarLabels(mt2_gads, varName = c("mc1", "mc2", "mc3"),
                           varLabel = c("Lang: Eng", "Aus spoken", "other"))

out <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"),
                            values = c("Aus", "Eng", "Eng"),
                            label_by_hand = c("other" = "mc3"))
```
