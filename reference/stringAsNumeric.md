# Transform string to numeric.

Transform a string variable within a `GADSdat` or `all_GADSdat` object
to a numeric variable.

## Usage

``` r
stringAsNumeric(GADSdat, varName)
```

## Arguments

- GADSdat:

  `GADSdat` object imported via `eatGADS`.

- varName:

  Character string of a variable name.

## Value

Returns the `GADSdat` object with with the changed variable.

## Details

Applied to a `GADSdat` or `all_GADSdat` object, this function uses
[`asNumericIfPossible`](https://weirichs.github.io/eatTools/reference/asNumericIfPossible.html)
to change the variable class and changes the `format` column in the meta
data.
