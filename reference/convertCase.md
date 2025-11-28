# Modify upper and lower case for strings.

Convert a character vector, all character variables in a `data.frame` or
selected variables in a `GADSdat` to upper (`"uppper"`), lower
(`"lower"`), or first letter upper and everything else lower case
(`"upperFirst"`).

## Usage

``` r
convertCase(x, case = c("lower", "upper", "upperFirst"), ...)

# S3 method for class 'GADSdat'
convertCase(x, case = c("lower", "upper", "upperFirst"), vars, ...)
```

## Arguments

- x:

  A character vector, `data.frame`, or `GADSdat`.

- case:

  Character vector of length 1. What case should the strings be
  converted to? Available options are `"lower"`, `"upper"`, or
  `"upperFirst"`.

- ...:

  further arguments passed to or from other methods.

- vars:

  Character vector. What variables in the `GADSdat` should the
  conversion be applied to?

## Value

Returns the converted object.

## Methods (by class)

- `convertCase(GADSdat)`: convert case for `GADSdats`

## Examples

``` r
# for character
convertCase(c("Hi", "HEllo", "greaT"), case = "upperFirst")
#> [1] "Hi"    "Hello" "Great"

# for GADSdat
input_g <- import_DF(data.frame(v1 = 1:3, v2 = c("Hi", "HEllo", "greaT"),
                          stringsAsFactors = FALSE))
convertCase(input_g, case = "upperFirst", vars = "v2")
#> $dat
#>   v1    v2
#> 1  1    Hi
#> 2  2 Hello
#> 3  3 Great
#> 
#> $labels
#>    varName varLabel format display_width labeled value valLabel missings
#> v1      v1     <NA>   <NA>            NA      no    NA     <NA>     <NA>
#> v2      v2     <NA>   <NA>            NA      no    NA     <NA>     <NA>
#> 
#> attr(,"class")
#> [1] "GADSdat" "list"   

```
