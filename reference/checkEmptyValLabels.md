# Check Value Labels

Check value labels for (a) value labels with no occurrence in the data
(`checkEmptyValLabels`) and (b) values with no value labels
(`checkMissingValLabels`).

## Usage

``` r
checkEmptyValLabels(
  GADSdat,
  vars = namesGADS(GADSdat),
  valueRange = NULL,
  output = c("list", "data.frame")
)

checkMissingValLabels(
  GADSdat,
  vars = namesGADS(GADSdat),
  classes = c("integer"),
  valueRange = NULL,
  output = c("list", "data.frame")
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- vars:

  Character vector with the variable names to which `checkValLabels()`
  should be applied.

- valueRange:

  \[optional\] Numeric vector of length 2: In which range should numeric
  values be checked? If specified, only numeric values are returned and
  strings are omitted.

- output:

  Should the output be structured as a `"list"` or a `"data.frame"`?

- classes:

  Character vector with the classes to which `checkMissingLabels()`
  should be applied. Valid options are `"integer"`, `"double"`, and
  `"character"`.

## Value

Returns a list of length `vars` or a `data.frame`.

## Details

`NAs` are excluded from this check. Designated missing codes are
reported normally.

## Functions

- `checkEmptyValLabels()`: check for superfluous value labels

- `checkMissingValLabels()`: check for missing value labels

## Examples

``` r
# Check a categorical and a metric variable
checkMissingValLabels(pisa, vars = c("g8g9", "age"))
#> $g8g9
#> NULL
#> 
checkEmptyValLabels(pisa, vars = c("g8g9", "age"))
#> $g8g9
#> NULL
#> 
#> $age
#> NULL
#> 

# Check while defining a specific value range
checkMissingValLabels(pisa, vars = c("g8g9", "age", "idschool"),
              valueRange = c(0, 5))
#> $g8g9
#> NULL
#> 
#> $idschool
#> $idschool$varLabel
#> [1] "School-ID"
#> 
#> $idschool$missing_labels
#> [1] 2 3 4 5
#> 
#> 
checkEmptyValLabels(pisa, vars = c("g8g9", "age", "idschool"),
              valueRange = c(0, 5))
#> $g8g9
#> NULL
#> 
#> $age
#> NULL
#> 
#> $idschool
#> NULL
#> 
```
