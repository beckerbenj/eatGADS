# Recode multiple choice variable with multiple variables.

Recode multiple variables (representing a single multiple choice item)
based on multiple character variables (representing a text field).

## Usage

``` r
collapseMultiMC_Text(
  GADSdat,
  mc_vars,
  text_vars,
  mc_var_4text,
  var_suffix = "_r",
  label_suffix = "(recoded)",
  invalid_miss_code = -98,
  invalid_miss_label = "Missing: Invalid response",
  notext_miss_code = -99,
  notext_miss_label = "Missing: By intention"
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- mc_vars:

  A character vector with the variable names of the multiple choice
  variable. Names of the character vector are the corresponding values
  that are represented by the individual variables. Creation by
  [`matchValues_varLabels`](https://beckerbenj.github.io/eatGADS/reference/matchValues_varLabels.md)
  is recommended.

- text_vars:

  A character vector with the names of the text variables which should
  be collapsed.

- mc_var_4text:

  The name of the multiple choice variable that signals that information
  from the text variable should be used. This variable is recoded
  according to the final status of the text variables.

- var_suffix:

  Variable suffix for the newly created `GADSdat`. If an empty
  character, the existing variables are overwritten.

- label_suffix:

  Suffix added to variable label for the newly created or modified
  variables in the `GADSdat`.

- invalid_miss_code:

  Missing code which is given to new character variables if all text
  entries where recoded into the dichotomous variables.

- invalid_miss_label:

  Value label for `invalid_miss_code`.

- notext_miss_code:

  Missing code which is given to empty character variables.

- notext_miss_label:

  Value label for `notext_miss_code`.

## Value

Returns a `GADSdat` containing the newly computed variables.

## Details

If a multiple choice item can be answered with ticking multiple boxes,
multiple variables in the data set are necessary to represent this item.
In this case, an additional text field for further answers can also
contain multiple values at once. However, some of the answers in the
text field might be redundant to the dummy variables.
`collapseMultiMC_Text` allows to recode multiple MC items of this kind
based on multiple text variables. The recoding can be prepared by
expanding the single text variable
([`createLookup`](https://beckerbenj.github.io/eatGADS/reference/createLookup.md)
and
[`applyLookup_expandVar`](https://beckerbenj.github.io/eatGADS/reference/applyLookup_expandVar.md))
and by matching the dummy variables to its underlying values stored in
variable labels
([`matchValues_varLabels`](https://beckerbenj.github.io/eatGADS/reference/matchValues_varLabels.md)).

The function recodes the dummy variables according to the character
variables. Additionally, the `mc_var_4text` variable is recoded
according to the final status of the `text_vars` (exception: if the text
variables were originally `NA`, `mc_var_4text` is left as it was).

Missing values in the character variables can be represented either by
`NAs` or by empty characters. The multiple choice variables specified
with `mc_vars` can only contain the values `0`, `1` and missing codes.
The value `1` must always represent "this category applies". If
necessary, use
[`recodeGADS`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
for recoding.

For cases for which the `text_vars` contain only values that can be
recoded into the `mc_vars`, all new `text_vars` are given specific
missing codes (see `invalid_miss_code` and `invalid_miss_label`). All
remaining `NAs` on the character variables are given a specific missing
code (`notext_miss_code`).

## Examples

``` r
# Prepare example data
mt2 <- data.frame(ID = 1:4, mc1 = c(1, 0, 0, 0), mc2 = c(0, 0, 0, 0), mc3 = c(0, 1, 1, 0),
                  text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, "Ger"),
                  stringsAsFactors = FALSE)
mt2_gads <- import_DF(mt2)
mt3_gads <- changeVarLabels(mt2_gads, varName = c("mc1", "mc2", "mc3"),
                            varLabel = c("Lang: Eng", "Aus spoken", "other"))

## All operations (see also respective help pages of functions for further explanations)
mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"),
            values = c("Aus", "Eng", "Eng"), label_by_hand = c("other" = "mc3"))

out_gads <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars,
             text_vars = c("text1", "text2"), mc_var_4text = "mc3")
#> No rows removed from meta data.
#> Adding meta data for the following variables: mc1_r, mc2_r, mc3_r, text1_r, text2_r

out_gads2 <- multiChar2fac(out_gads, vars = c("text1_r", "text2_r"))

final_gads <- remove2NAchar(out_gads2, vars = c("text1_r_r", "text2_r_r"),
                              max_num = 1, na_value = -99, na_label = "missing: excessive answers")
#> Removing the following rows from meta data: text2_r_r
#> No rows added to meta data.
```
