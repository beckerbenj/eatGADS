# Create a composite variable.

Create a composite variable out of two variables.

## Usage

``` r
composeVar(GADSdat, sourceVars, primarySourceVar, newVar, checkVarName = TRUE)
```

## Arguments

- GADSdat:

  `GADSdat` or `all_GADSdat` object imported via eatGADS.

- sourceVars:

  Character vector of length two containing the variable names which
  represent the sources of information.

- primarySourceVar:

  Character vector containing a single variable name. Which of the
  `sourceVars` should be preferred?

- newVar:

  Character vector containing the name of the new composite variable.

- checkVarName:

  Logical. Should `newVar` be checked by
  [`checkVarNames`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)?

## Value

The modified `GADSdat`.

## Details

A common use case for creating a composite variable is if there are
multiple sources for the same information. For example, a child and the
parents are asked about the child's native language. In such cases a
composite variable contains information from both variables, meaning
that one source is preferred and the other source is used to substitute
missing values.

## Examples

``` r
# example data
dat <- data.frame(ID = 1:4,
nat_lang_child = c("Engl", "Ger", "missing", "missing"),
nat_lang_father = c("Engl", "Engl", "Ger", "missing"),
stringsAsFactors = TRUE)
gads <- import_DF(dat)
changeMissings(gads, "nat_lang_child", value = 3, missings = "miss")
#> $dat
#>   ID nat_lang_child nat_lang_father
#> 1  1              1               1
#> 2  2              2               1
#> 3  3              3               2
#> 4  4              3               3
#> 
#> $labels
#>           varName varLabel format display_width labeled value valLabel missings
#> 1              ID     <NA>   <NA>            NA      no    NA     <NA>     <NA>
#> 2  nat_lang_child     <NA>   <NA>            NA     yes     1     Engl    valid
#> 3  nat_lang_child     <NA>   <NA>            NA     yes     2      Ger    valid
#> 4  nat_lang_child     <NA>   <NA>            NA     yes     3  missing     miss
#> 5 nat_lang_father     <NA>   <NA>            NA     yes     1     Engl    valid
#> 6 nat_lang_father     <NA>   <NA>            NA     yes     2      Ger    valid
#> 7 nat_lang_father     <NA>   <NA>            NA     yes     3  missing    valid
#> 
#> attr(,"class")
#> [1] "GADSdat" "list"   
changeMissings(gads, "nat_lang_father", value = 3, missings = "miss")
#> $dat
#>   ID nat_lang_child nat_lang_father
#> 1  1              1               1
#> 2  2              2               1
#> 3  3              3               2
#> 4  4              3               3
#> 
#> $labels
#>           varName varLabel format display_width labeled value valLabel missings
#> 1              ID     <NA>   <NA>            NA      no    NA     <NA>     <NA>
#> 2  nat_lang_child     <NA>   <NA>            NA     yes     1     Engl    valid
#> 3  nat_lang_child     <NA>   <NA>            NA     yes     2      Ger    valid
#> 4  nat_lang_child     <NA>   <NA>            NA     yes     3  missing    valid
#> 5 nat_lang_father     <NA>   <NA>            NA     yes     1     Engl    valid
#> 6 nat_lang_father     <NA>   <NA>            NA     yes     2      Ger    valid
#> 7 nat_lang_father     <NA>   <NA>            NA     yes     3  missing     miss
#> 
#> attr(,"class")
#> [1] "GADSdat" "list"   

# compose variable
composeVar(gads, sourceVars = c("nat_lang_child", "nat_lang_father"),
           primarySourceVar = "nat_lang_child", newVar = "nat_lang_comp")
#> $dat
#>   ID nat_lang_comp nat_lang_child nat_lang_father
#> 1  1             1              1               1
#> 2  2             2              2               1
#> 3  3             3              3               2
#> 4  4             3              3               3
#> 
#> $labels
#>            varName varLabel format display_width labeled value valLabel
#> 1               ID     <NA>   <NA>            NA      no    NA     <NA>
#> 8    nat_lang_comp     <NA>   <NA>            NA     yes     1     Engl
#> 9    nat_lang_comp     <NA>   <NA>            NA     yes     2      Ger
#> 10   nat_lang_comp     <NA>   <NA>            NA     yes     3  missing
#> 2   nat_lang_child     <NA>   <NA>            NA     yes     1     Engl
#> 3   nat_lang_child     <NA>   <NA>            NA     yes     2      Ger
#> 4   nat_lang_child     <NA>   <NA>            NA     yes     3  missing
#> 5  nat_lang_father     <NA>   <NA>            NA     yes     1     Engl
#> 6  nat_lang_father     <NA>   <NA>            NA     yes     2      Ger
#> 7  nat_lang_father     <NA>   <NA>            NA     yes     3  missing
#>    missings
#> 1      <NA>
#> 8     valid
#> 9     valid
#> 10    valid
#> 2     valid
#> 3     valid
#> 4     valid
#> 5     valid
#> 6     valid
#> 7     valid
#> 
#> attr(,"class")
#> [1] "GADSdat" "list"   

```
