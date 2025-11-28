# Drop duplicate IDs in a `GADSdat`.

Drop rows with duplicate IDs in a `GADSdat` object based on numbers of
missing values.

## Usage

``` r
dropDuplicateIDs(GADSdat, ID, varNames = setdiff(namesGADS(GADSdat), ID))
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- ID:

  Name of the ID variable.

- varNames:

  Character vector of variable names: Sum of missing values on these
  variables decide which rows are kept. Per default, all variables
  except the ID variable are used.

## Value

Returns the `GADSdat` with duplicate ID rows removed.

## Details

If duplicate IDs occur, it is often desirable to keep the row with the
least missing information. Therefore, `dropDuplicateIDs` drops rows
based on number of missing values on the specified variables
(`varNames`).

If multiple rows have the same number of missing values, a warning is
issued and the first of the respective rows is kept.

## Examples

``` r
# create example data set
gads_ori <- import_DF(data.frame(id_var = c(1, 2, 5, 4, 4),
  var1 = c(1, 2, -99, 1, -99)))
gads_ori <- changeMissings(gads_ori, varName = "var1",
  value = -99, missings = "miss")

# drop duplicate IDs
dropDuplicateIDs(gads_ori, ID = "id_var")
#> $dat
#>   id_var var1
#> 1      1    1
#> 2      2    2
#> 3      5  -99
#> 4      4    1
#> 
#> $labels
#>   varName varLabel format display_width labeled value valLabel missings
#> 1  id_var     <NA>   <NA>            NA      no    NA     <NA>     <NA>
#> 2    var1     <NA>   <NA>            NA     yes   -99     <NA>     miss
#> 
#> attr(,"class")
#> [1] "GADSdat" "list"   
```
