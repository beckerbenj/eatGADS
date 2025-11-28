# Shorten multiple text variables while giving NA codes.

Shorten text variables from a certain number on while coding overflowing
answers as complete missings.

## Usage

``` r
remove2NAchar(GADSdat, vars, max_num = 2, na_value, na_label)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- vars:

  A character vector with the names of the text variables.

- max_num:

  Maximum number of text variables. Additional text variables will be
  removed and NA codes given accordingly.

- na_value:

  Which NA value should be given in cases of too many values on text
  variables.

- na_label:

  Which value label should be given to the `na_value`.

## Value

Returns the modified `GADSdat`.

## Details

In some cases, multiple text variables contain the information of one
variable (e.g. multiple answers to an open item). If this is a case,
sometimes the number text variables displaying this variable should be
limited. `remove2NAchar` allows shortening multiple character variables,
this means character variables after `max_num` are removed from the
`GADSdat`. Cases, which had valid responses on these removed variables
are coded as missings (using `na_value` and `na_label`).

## Examples

``` r
## create an example GADSdat
example_df <- data.frame(ID = 1:4,
                        citizenship1 = c("German", "English", "missing by design", "Chinese"),
                        citizenship2 = c(NA, "German", "missing by design", "Polish"),
                        citizenship3 = c(NA, NA, NA, "German"),
                        stringsAsFactors = FALSE)
gads <- import_DF(example_df)

## shorten character variables
gads2 <- remove2NAchar(gads, vars = c("citizenship1", "citizenship2", "citizenship3"),
                      na_value = -99, na_label = "missing: too many answers")
#> Removing the following rows from meta data: citizenship3
#> No rows added to meta data.

```
