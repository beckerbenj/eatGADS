# Change missing code.

Change or add missing codes of one or multiple variables as part of a
`GADSdat` object.

## Usage

``` r
changeMissings(GADSdat, varName, value, missings)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varName:

  Character vector containing variable names.

- value:

  Numeric values.

- missings:

  Character vector of the new missing codes, either `"miss"` or
  `"valid"`. Missings tags are applied in the same ordering as `value`.

## Value

Returns the `GADSdat` object with changed meta data.

## Details

Applied to a `GADSdat` or `all_GADSdat` object, this function is a
wrapper of
[`getChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/getChangeMeta.md)
and
[`applyChangeMeta`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md).
The function supports changing multiple missing tags (`missings`) as
well as missing tags of multiple variables (`varName`) at once.

## Examples

``` r
# Set a specific value to missing
pisa2 <- changeMissings(pisa, varName = "computer_age",
                        value = 5, missings = "miss")

# Set multiple values to missing
pisa3 <- changeMissings(pisa, varName = "computer_age",
                        value = 1:4,
                        missings = c("miss", "miss", "miss", "miss"))

# Set a specific value to not missing
pisa4 <- changeMissings(pisa2, varName = "computer_age",
                        value = 5, missings = "valid")

# Add missing tags to multiple variables
pisa5 <- changeMissings(pisa, varName = c("g8g9", "computer_age"),
                        value = c(-99, -98), missings = c("miss", "miss"))
```
