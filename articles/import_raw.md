# Handling meta data

When trying to understand data, most often not only the actual data is
required, but also so called meta data. Meta data usually includes:

- variable labels: Labels describing, what a variable in the data
  represents
- value labels: Labels describing, what a certain value on a certain
  variable represents

While the `data.frame` class in `R` supports value labels to a certain
degree with the `factor` class, its functionality is limited. Other data
formats like `.xlsx` or `.csv` support no meta data at all. Commercial
software like `SPSS` provides such functionality but can not compete
with the various tools for analyzing data that `R` provides.

`eatGADS` is an `R` package that was developed to bridge this gap. Its
main purpose is providing a data format in `R` specifically designed for
storing meta data together with data in one place. Therefore it provides
an `S3` class called `GADSdat`. The following vignette concentrates on
how to import data into the `GADSdat` format and work with it in the `R`
environment. In collaboration with the
`IQB Forschungsdatenzentrum (FDZ)` the package can also be used to
distribute data.

Note that `eatGADS` also allows the handling of large hierarchical data
structures via relational data bases. This functionality is explained in
more detail in an additional vignette.

## Setup

The package can be installed from GitHub. Note that older R versions had
issues with installations from online repositories like GitHub. `R`
version `> 3.6.0` should work without any issues.

``` r
devtools::install_github("beckerbenj/eatGADS")
```

``` r
# loading the package
library(eatGADS)
```

## Importing data into the `GADSdat` format

### Importing from SPSS

`R` offers a variety of tools to import data from all sorts of data
formats. `SPSS` data (`.sav` files) can be imported directly into the
`GADSdat` format, with `haven` used as a backend. Note that this is the
easiest way to import data into the `GADSdat` format.

``` r
# importing an SPSS file
gads <- import_spss("path/example.sav")
```

### Importing from Excel etc.

All other file types should be imported into `R` first and then supplied
as `data.frames` to `import_raw`. Below is a small selection of
functions that import data as `data.frames`. For an extensive overview
of importing functions using the package `readr` see also [this book
chapter](https://r4ds.had.co.nz/data-import.html), while the package
`readxl` is explained in more detail on this \[homepage\]
(<https://readxl.tidyverse.org/>). As these files are plain data files,
meta data has to be supplied as separate data sheets.

Note that none of the `data.frames` can contain variables of the class
`factor`, as this in itself constitutes meta data. If using base `R` to
import data make sure to use the argument `stringsAsFactors = FALSE`. If
necessary, convert `factors` to character via `as.character`.

``` r
# importing text files
input_txt <- read.table("path/example.txt", stringsAsFactors = FALSE)
# importing German csv files (; separated)
input_csv <- read.csv2("path/example.csv", stringsAsFactors = FALSE)
# importing Excel files
input_xlsx <- readxl::read_excel("path/example.xlsx")
```

`import_raw` takes three separate `data.frames` as input. The actual
data set (`df`), the variable labels (`varLabels`) and the value labels
(`valLabels`). These three objects have to be supplied in a very
specific format.

The `varLabels` object has to contain two variables: `varName`, which
should exactly correspond to the variable names in `df` and `varLabels`
which should contain the desired variable labels as strings. Note that
this `data.frame` should contain as many rows as there are variables in
`df`.

The optional `valLabels` object has to contain four variables:
`varName`, which should exactly correspond to the variable names in
`df`; `values`, which should correspond to the respective values in `df`
and has to be a numeric vector (labels for character vectors are
currently not supported); `valLabels`, which should contain the value
labels as strings; and `missings`, a column indicating whether the value
indicates a missing value. Valid values for `missings` are `"valid"` =
no missing code and `"miss"` = missing code. Note that this `data.frame`
can not contain any `varNames` that are not variables in `df`. However,
not all variables in `df` have to occur in `valLabels`.

``` r
# Example data set
df <- data.frame(ID = 1:4, sex = c(0, 0, 1, 1), 
                 forename = c("Tim", "Bill", "Ann", "Chris"), stringsAsFactors = FALSE)
# Example variable labels
varLabels <- data.frame(varName = c("ID", "sex", "forename"), 
                        varLabel = c("Person Identifier", "Sex as self reported", 
                                     "first name as reported by teacher"), 
                        stringsAsFactors = FALSE)
# Example value labels
valLabels <- data.frame(varName = rep("sex", 3), 
                        value = c(0, 1, -99), 
                        valLabel = c("male", "female", "missing - omission"), 
                        missings = c("valid", "valid", "miss"), stringsAsFactors = FALSE)

df
#>   ID sex forename
#> 1  1   0      Tim
#> 2  2   0     Bill
#> 3  3   1      Ann
#> 4  4   1    Chris
varLabels
#>    varName                          varLabel
#> 1       ID                 Person Identifier
#> 2      sex              Sex as self reported
#> 3 forename first name as reported by teacher
valLabels
#>   varName value           valLabel missings
#> 1     sex     0               male    valid
#> 2     sex     1             female    valid
#> 3     sex   -99 missing - omission     miss

# import 
gads <- import_raw(df = df, varLabels = varLabels, valLabels = valLabels)
```

## `GADSdat` class

The resulting object is of the class `GADSdat` and contains a data sheet
and a meta data sheet.

``` r
# Inpsect resulting object 
gads 
#> $dat
#>   ID sex forename
#> 1  1   0      Tim
#> 2  2   0     Bill
#> 3  3   1      Ann
#> 4  4   1    Chris
#> 
#> $labels
#>    varName                          varLabel format display_width labeled value
#> 1       ID                 Person Identifier   <NA>            NA      no    NA
#> 2      sex              Sex as self reported   <NA>            NA     yes   -99
#> 3      sex              Sex as self reported   <NA>            NA     yes     0
#> 4      sex              Sex as self reported   <NA>            NA     yes     1
#> 5 forename first name as reported by teacher   <NA>            NA      no    NA
#>             valLabel missings
#> 1               <NA>     <NA>
#> 2 missing - omission     miss
#> 3               male    valid
#> 4             female    valid
#> 5               <NA>     <NA>
#> 
#> attr(,"class")
#> [1] "GADSdat" "list"
```

### Saving `GADSdat` objects

`GADSdat` objects, for example, can be saved as `RDS` files. This is
also the preferred data format for distributing `GADSdat` objects to the
`FDZ`.

``` r
# Inpsect resulting object 
saveRDS(gads, "path/gads.RDS")
```

### Using `GADSdat` objects in R

`eatGADS` provides convenient functions for extracting data and meta
data from `GADSdat` objects. `extractMeta` is used to access the meta
data for specific variables (or all variables, if no specific variable
name is provided).

``` r
# Inpsect resulting object 
extractMeta(gads, vars = c("sex"))
#>   varName             varLabel format display_width labeled value
#> 2     sex Sex as self reported   <NA>            NA     yes   -99
#> 3     sex Sex as self reported   <NA>            NA     yes     0
#> 4     sex Sex as self reported   <NA>            NA     yes     1
#>             valLabel missings
#> 2 missing - omission     miss
#> 3               male    valid
#> 4             female    valid
extractMeta(gads)
#>    varName                          varLabel format display_width labeled value
#> 1       ID                 Person Identifier   <NA>            NA      no    NA
#> 2      sex              Sex as self reported   <NA>            NA     yes   -99
#> 3      sex              Sex as self reported   <NA>            NA     yes     0
#> 4      sex              Sex as self reported   <NA>            NA     yes     1
#> 5 forename first name as reported by teacher   <NA>            NA      no    NA
#>             valLabel missings
#> 1               <NA>     <NA>
#> 2 missing - omission     miss
#> 3               male    valid
#> 4             female    valid
#> 5               <NA>     <NA>
```

`extractData` is used to extract data. With its arguments the structure
of the resulting data can be defined. If `convertMiss = TRUE`, which is
the default, is used, values that are listed as missing codes are
recoded to `NAs`. With the `convertLabels` argument it can be specified
how value labels should be used. If set to `"character"` all labeled
values are recoded to character, the same applies to “`factor`”. If set
to `"numeric"`, the value labels are not applied.

``` r
# Extract data without applying labels
dat1 <- extractData(gads, convertMiss = TRUE, convertLabels = "numeric")
dat1
#>   ID sex forename
#> 1  1   0      Tim
#> 2  2   0     Bill
#> 3  3   1      Ann
#> 4  4   1    Chris

dat2 <- extractData(gads, convertMiss = TRUE, convertLabels = "character")
dat2
#>   ID    sex forename
#> 1  1   male      Tim
#> 2  2   male     Bill
#> 3  3 female      Ann
#> 4  4 female    Chris
```

## Modifying `GADSdat` objects

`GADSdat` objects can also be modified even though only a certain amount
of operations are supported. For smaller changes to the data and meta
data a number of convenience functions exists. These functions allow
modifying variable labels (`changeVarLabels`), modifying variable names
(`changeVarNames`) and recoding values (`recodeGADS`).

``` r
### wrapper functions
# Modify variable labels
gads2 <- changeVarLabels(gads, varName = c("ID"), varLabel = c("Test taker ID"))
extractMeta(gads2, vars = "ID")
#>   varName      varLabel format display_width labeled value valLabel missings
#> 1      ID Test taker ID   <NA>            NA      no    NA     <NA>     <NA>

# Modify variable name
gads3 <- changeVarNames(gads, oldNames = c("ID"), newNames = c("idstud"))
extractMeta(gads3, vars = "idstud")
#>   varName          varLabel format display_width labeled value valLabel
#> 1  idstud Person Identifier   <NA>            NA      no    NA     <NA>
#>   missings
#> 1     <NA>
extractData(gads3)
#>   idstud    sex forename
#> 1      1   male      Tim
#> 2      2   male     Bill
#> 3      3 female      Ann
#> 4      4 female    Chris

# recode GADS
gads4 <- recodeGADS(gads, varName = "sex", oldValues = c(0, 1, -99), newValues = c(1, 2, 99))
extractMeta(gads4, vars = "sex")
#>   varName             varLabel format display_width labeled value
#> 2     sex Sex as self reported   <NA>            NA     yes     1
#> 3     sex Sex as self reported   <NA>            NA     yes     2
#> 4     sex Sex as self reported   <NA>            NA     yes    99
#>             valLabel missings
#> 2               male    valid
#> 3             female    valid
#> 4 missing - omission     miss
extractData(gads4, convertLabels = "numeric")
#>   ID sex forename
#> 1  1   1      Tim
#> 2  2   1     Bill
#> 3  3   2      Ann
#> 4  4   2    Chris
```

For simultaneous changes to multiple variables a set of functions is
implemented that extract a table for changes and applies the changes as
written into this change table. To enable an easier work flow the change
table could also be saved as an Excel file, modified via Excel and again
imported into `R`. See the help pages of the respective functions for
more details.

``` r
# extract changeTable
varChanges <- getChangeMeta(gads, level = "variable")
# modify changeTable
varChanges[varChanges$varName == "ID", "varLabel_new"] <- "Test taker ID"
# Apply changes
gads5 <- applyChangeMeta(varChanges, gads)
extractMeta(gads5, vars = "ID")
#>   varName      varLabel format display_width labeled value valLabel missings
#> 1      ID Test taker ID   <NA>            NA      no    NA     <NA>     <NA>
```

## Writing SPSS files

Objects of the class `GADSdat` can also be exported into the SPSS
format, utilizing `haven`. Note that this function is slightly
experimental and problems with specific character strings might occur.

``` r
write_spss(gads, "path/example_out.sav")
```

If the `haven` format is preferred for working in `R`, a `GADSdat`
object can also be transformed to its equivalent `tibble` format, as if
the data was imported from SPSS via `haven`.

``` r
haven_dat <- export_tibble(gads)
haven_dat
#> # A tibble: 4 × 3
#>      ID sex        forename
#>   <dbl> <hvn_lbl_> <chr>   
#> 1     1 0          Tim     
#> 2     2 0          Bill    
#> 3     3 1          Ann     
#> 4     4 1          Chris
lapply(haven_dat, attributes)
#> $ID
#> $ID$label
#> [1] "Person Identifier"
#> 
#> 
#> $sex
#> $sex$label
#> [1] "Sex as self reported"
#> 
#> $sex$na_values
#> [1] -99
#> 
#> $sex$class
#> [1] "haven_labelled_spss" "haven_labelled"     
#> 
#> $sex$labels
#> missing - omission               male             female 
#>                -99                  0                  1 
#> 
#> 
#> $forename
#> $forename$label
#> [1] "first name as reported by teacher"
#> 
#> $forename$na_values
#> character(0)
```
