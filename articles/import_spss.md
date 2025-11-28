# \`import_spss\`: Importing data from 'SPSS'

[`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
allows importing data from `SPSS` (`.sav` and `.zsav` files) into `R` by
using the `R` package `haven`.

This vignette illustrates a typical workflow of importing a `SPSS` file
using
[`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
and
[`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md).
For illustrative purposes we use a small example data set from the
campus files of the German PISA Plus assessment. The complete campus
files and the original data set can be accessed
[here](https://www.iqb.hu-berlin.de/fdz/Datenzugang/CF-Antrag/AntragsformularCF)
and [here](https://www.iqb.hu-berlin.de/fdz/Datenzugang/SUF-Antrag).

## Importing

``` r
library(eatGADS)
```

We can import an `.sav` data set via the
[`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
function. Checks on variable names (for data base compatibility) are
performed automatically. Changes to the variable names are reported to
the console. This behavior can be suppressed by setting
`checkVarNames = FALSE`.

``` r
sav_path <- system.file("extdata", "pisa.zsav", package = "eatGADS")
gads_obj <- import_spss(sav_path)
```

## `GADSdat` objects

The resulting object is of the class `GADSdat`. It is basically a named
list containing the actual data (`dat`) and the meta data (`labels`).

``` r
class(gads_obj)
#> [1] "GADSdat" "list"
names(gads_obj)
#> [1] "dat"    "labels"
```

The names of the variables in a `GADSdat` object can be accessed via the
[`namesGADS()`](https://beckerbenj.github.io/eatGADS/reference/namesGADS.md)
function. The meta data of variables can be accessed via the
[`extractMeta()`](https://beckerbenj.github.io/eatGADS/reference/extractMeta.md)
function.

``` r
namesGADS(gads_obj)
#>   [1] "idstud"       "idschool"     "idclass"      "schtype"      "sameteach"   
#>   [6] "g8g9"         "ganztag"      "classsize"    "repeated"     "gender"      
#>  [11] "age"          "language"     "migration"    "hisced"       "hisei"       
#>  [16] "homepos"      "books"        "pared"        "computer_age" "internet_age"
#>  [21] "int_use_a"    "int_use_b"    "truancy_a"    "truancy_b"    "truancy_c"   
#>  [26] "int_a"        "int_b"        "int_c"        "int_d"        "instmot_a"   
#>  [31] "instmot_b"    "instmot_c"    "instmot_d"    "norms_a"      "norms_b"     
#>  [36] "norms_c"      "norms_d"      "norms_e"      "norms_f"      "anxiety_a"   
#>  [41] "anxiety_b"    "anxiety_c"    "anxiety_d"    "anxiety_e"    "selfcon_a"   
#>  [46] "selfcon_b"    "selfcon_c"    "selfcon_d"    "selfcon_e"    "worketh_a"   
#>  [51] "worketh_b"    "worketh_c"    "worketh_d"    "worketh_e"    "worketh_f"   
#>  [56] "worketh_g"    "worketh_h"    "worketh_i"    "intent_a"     "intent_b"    
#>  [61] "intent_c"     "intent_d"     "intent_e"     "behav_a"      "behav_b"     
#>  [66] "behav_c"      "behav_d"      "behav_e"      "behav_f"      "behav_g"     
#>  [71] "behav_h"      "teach_a"      "teach_b"      "teach_c"      "teach_d"     
#>  [76] "teach_e"      "cognact_a"    "cognact_b"    "cognact_c"    "cognact_d"   
#>  [81] "cognact_e"    "cognact_f"    "cognact_g"    "cognact_h"    "cognact_i"   
#>  [86] "discpline_a"  "discpline_b"  "discpline_c"  "discpline_d"  "discpline_e" 
#>  [91] "relation_a"   "relation_b"   "relation_c"   "relation_d"   "relation_e"  
#>  [96] "belong_a"     "belong_b"     "belong_c"     "belong_d"     "belong_e"    
#> [101] "belong_f"     "belong_g"     "belong_h"     "belong_i"     "attitud_a"   
#> [106] "attitud_b"    "attitud_c"    "attitud_d"    "attitud_e"    "attitud_f"   
#> [111] "attitud_g"    "attitud_h"    "grade_de"     "grade_ma"     "grade_bio"   
#> [116] "grade_che"    "grade_phy"    "grade_sci"    "ma_pv1"       "ma_pv2"      
#> [121] "ma_pv3"       "ma_pv4"       "ma_pv5"       "rea_pv1"      "rea_pv2"     
#> [126] "rea_pv3"      "rea_pv4"      "rea_pv5"      "sci_pv1"      "sci_pv2"     
#> [131] "sci_pv3"      "sci_pv4"      "sci_pv5"
extractMeta(gads_obj, vars = c("schtype", "idschool"))
#>    varName     varLabel format display_width labeled value
#> 2 idschool    School-ID   F8.0            NA      no    NA
#> 4  schtype School track   F8.0            NA     yes     1
#> 5  schtype School track   F8.0            NA     yes     2
#> 6  schtype School track   F8.0            NA     yes     3
#>                                    valLabel missings
#> 2                                      <NA>     <NA>
#> 4                Gymnasium (academic track)    valid
#> 5                                Realschule    valid
#> 6 schools with several courses of education    valid
```

Commonly, the most informative columns are `varLabel` (containing
variable labels), `value` (referencing labeled values), `valLabel`
(containing value labels) and `missings` (missing tag: is a labeled
value a missing value (`"miss"`) or not (`"valid"`)).

## Extracting data from `GADSdat`

If we want to use the data for analyses in `R` we have to extract it
from the `GADSdat` object via the function
[`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md).
In doing so, we have to make two important decisions: (a) how should
values marked as missing values be treated (`convertMiss`)? And (b) how
should labeled values in general be treated (`labels2character`,
`labels2factor`, `labels2ordered`, `dropPartialLabels`)?

If a variable name is not provided under any of `labels2character`,
`labels2factor`, `labels2ordered`, all value labels of the corresponding
variable are simply dropped. If a variable name is provided under
`labels2character`, the value labels of the corresponding variable are
applied and the resulting variable is a character variable.
`labels2factor` converts variables to factor and `labels2ordered`
converts variables to ordered factors.

See
[`?extractData2`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
for more details.

``` r
## convert all labeled variables to character
dat1 <- extractData2(gads_obj, labels2character = namesGADS(gads_obj))
dat1[1:5, 1:10]
#>   idstud idschool idclass                                   schtype sameteach
#> 1      1      127     392                                Realschule       Yes
#> 2      2       65     201 schools with several courses of education        No
#> 3      3       10      34                Gymnasium (academic track)        No
#> 4      4      103     319 schools with several courses of education       Yes
#> 5      5       57     179                                Realschule       Yes
#>                     g8g9 ganztag classsize               repeated gender
#> 1                   <NA>      No         9 Did not repeat a grade Female
#> 2                   <NA>      No        10 Did not repeat a grade Female
#> 3 G8 - 8 years to abitur      No        28 Did not repeat a grade   Male
#> 4                   <NA>      No        12 Did not repeat a grade   Male
#> 5                   <NA>     Yes        25 Did not repeat a grade Female

## leave labeled variables as numeric
dat2 <- extractData2(gads_obj)
dat2[1:5, 1:10]
#>   idstud idschool idclass schtype sameteach g8g9 ganztag classsize repeated
#> 1      1      127     392       2         2   NA       1         9        1
#> 2      2       65     201       3         1   NA       1        10        1
#> 3      3       10      34       1         1    1       1        28        1
#> 4      4      103     319       3         2   NA       1        12        1
#> 5      5       57     179       2         2   NA       2        25        1
#>   gender
#> 1      1
#> 2      1
#> 3      2
#> 4      2
#> 5      1

## leave labeled variables as numeric but convert some variables to character and some to factor
dat3 <- extractData2(gads_obj, labels2character = c("gender", "language"),
                     labels2factor = c("schtype", "sameteach"))
dat3[1:5, 1:10]
#>   idstud idschool idclass                                   schtype sameteach
#> 1      1      127     392                                Realschule       Yes
#> 2      2       65     201 schools with several courses of education        No
#> 3      3       10      34                Gymnasium (academic track)        No
#> 4      4      103     319 schools with several courses of education       Yes
#> 5      5       57     179                                Realschule       Yes
#>   g8g9 ganztag classsize repeated gender
#> 1   NA       1         9        1 Female
#> 2   NA       1        10        1 Female
#> 3    1       1        28        1   Male
#> 4   NA       1        12        1   Male
#> 5   NA       2        25        1 Female
```

In general, we recommend leaving labeled variables as numeric and
converting values with missing codes to `NA`. Both are the default
behavior for
[`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md).
If required, value labels can always be accessed via using
[`extractMeta()`](https://beckerbenj.github.io/eatGADS/reference/extractMeta.md)
on the `GADSdat` object or the data base.
