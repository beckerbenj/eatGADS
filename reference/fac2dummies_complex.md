# Transform a complex factor variable to dummy variables.

Convert a factor variable with complex factor levels (factor levels
contain combinations of other factor levels) to dummy variables. Dummy
variables are coded `1` (`"yes"`) and `0` (`"no"`).

## Usage

``` r
fac2dummies_complex(GADSdat, var)
```

## Arguments

- GADSdat:

  A `data.frame` or `GADSdat` object.

- var:

  A character vector with the name of the factor variable.

## Value

Returns a `GADSdat` containing the newly computed variables.

## Details

The basic functionality of this function is analogous to
[`fac2dummies`](https://beckerbenj.github.io/eatGADS/reference/fac2dummies.md).
However, the function expects factor levels to only go to `9`. Higher
numbers are treated as combinations of factor levels, for example `"13"`
as `"1"` and `"3"`.

## Examples

``` r
## create an example GADSdat
df_fac <- data.frame(id = 1:6, fac = c("Opt a", "Opt c, Opt b", "Opt c",
"Opt b", "Opt a, Opt b", "Opt a, Opt b, Opt c"), stringsAsFactors = TRUE)
g_fac <- import_DF(df_fac)
g_fac <- recodeGADS(g_fac, varName = "fac", oldValues = c(1, 2, 3, 4, 5, 6),
                     newValues = c(1, 12, 123, 2, 3, 23))

## transform factor variable
fac2dummies_complex(g_fac, "fac")
#> The following dummy variables have been created: fac_a, fac_b, fac_c
#> $dat
#>   id fac fac_a fac_b fac_c
#> 1  1   1     1     0     0
#> 2  2  23     0     1     1
#> 3  3   3     0     0     1
#> 4  4   2     0     1     0
#> 5  5  12     1     1     0
#> 6  6 123     1     1     1
#> 
#> $labels
#>    varName   varLabel format display_width labeled value            valLabel
#> 1       id       <NA>   <NA>            NA      no    NA                <NA>
#> 2      fac       <NA>   <NA>            NA     yes     1               Opt a
#> 3      fac       <NA>   <NA>            NA     yes     2               Opt b
#> 4      fac       <NA>   <NA>            NA     yes     3               Opt c
#> 5      fac       <NA>   <NA>            NA     yes    12        Opt a, Opt b
#> 6      fac       <NA>   <NA>            NA     yes    23        Opt c, Opt b
#> 7      fac       <NA>   <NA>            NA     yes   123 Opt a, Opt b, Opt c
#> 8    fac_a fac: Opt a   F2.0            NA     yes     0                  no
#> 9    fac_a fac: Opt a   F2.0            NA     yes     1                 yes
#> 10   fac_b fac: Opt b   F2.0            NA     yes     0                  no
#> 11   fac_b fac: Opt b   F2.0            NA     yes     1                 yes
#> 12   fac_c fac: Opt c   F2.0            NA     yes     0                  no
#> 13   fac_c fac: Opt c   F2.0            NA     yes     1                 yes
#>    missings
#> 1      <NA>
#> 2     valid
#> 3     valid
#> 4     valid
#> 5     valid
#> 6     valid
#> 7     valid
#> 8     valid
#> 9     valid
#> 10    valid
#> 11    valid
#> 12    valid
#> 13    valid
#> 
#> attr(,"class")
#> [1] "GADSdat" "list"   

```
