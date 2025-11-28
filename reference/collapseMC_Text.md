# Recode a multiple choice variable according to a character variable.

Recode an labeled integer variable (based on an multiple choice item),
according to a character variable (e.g. an open answer item).

## Usage

``` r
collapseMC_Text(
  GADSdat,
  mc_var,
  text_var,
  mc_code4text,
  var_suffix = "_r",
  label_suffix = "(recoded)"
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- mc_var:

  The variable name of the multiple choice variable.

- text_var:

  The variable name of the text variable.

- mc_code4text:

  The value label in `mc_var` that indicates that information from the
  text variable should be used.

- var_suffix:

  Variable name suffix for the newly created variables. If `NULL`,
  variables are overwritten.

- label_suffix:

  Variable label suffix for the newly created variable (only added in
  the meta data). If `NULL` no suffix is added.

## Value

Returns a `GADSdat` containing the newly computed variable.

## Details

Multiple choice variables can be represented as labeled integer
variables in a `GADSdat`. Multiple choice items with a forced choice
frequently contain an open answer category. However, sometimes open
answers overlap with the existing categories in the multiple choice
item. `collapseMC_Text` allows recoding the multiple choice variable
based on the open answer variable.

`mc_code4text` indicates when entries in the `text_var` should be used.
Additionally, entries in the `text_var` are also used when there are
missings on the `mc_var`. New values for the `mc_var` are added in the
meta data, while preserving the initial ordering of the value labels.
Newly added value labels are sorted alphabetically.

For more details see the help vignette:
[`vignette("recoding_forcedChoice", package = "eatGADS")`](https://beckerbenj.github.io/eatGADS/articles/recoding_forcedChoice.md).

## Examples

``` r
# Example gads
example_df <- data.frame(ID = 1:5, mc = c("blue", "blue", "green", "other", "other"),
                        open = c(NA, NA, NA, "yellow", "blue"),
                        stringsAsFactors = FALSE)
example_df$mc <- as.factor(example_df$mc)
gads <- import_DF(example_df)

# recode
gads2 <- collapseMC_Text(gads, mc_var = "mc", text_var = "open",
                        mc_code4text = "other")
#> No rows removed from meta data.
#> Adding meta data for the following variables: mc_r
```
