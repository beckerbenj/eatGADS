# \`getGADS\`: Using a relational eatGADS data base

This vignette illustrates how a relational `eatGADS` data base can be
accessed and used. Therefore, the vignette is targeted at users who make
use of an existing data base.

For illustrative purposes we use a small example data base based on the
campus files of the German PISA Plus assessment. The complete campus
files and the original data set can be accessed
[here](https://www.iqb.hu-berlin.de/fdz/Datenzugang/CF-Antrag/AntragsformularCF)
and [here](https://www.iqb.hu-berlin.de/fdz/Datenzugang/SUF-Antrag). The
data base is installed alongside `eatGADS` and the path can be accessed
via the [`system.file()`](https://rdrr.io/r/base/system.file.html)
function.

``` r
library(eatGADS)
db_path <- system.file("extdata", "pisa.db", package = "eatGADS")
db_path
#> [1] "/home/runner/work/_temp/Library/eatGADS/extdata/pisa.db"
```

## Why?

Relational data bases created by `eatGADS` provide an alternative way of
storing hierarchically structured data (e.g. from educational
large-scale assessments). Compared to conventional approaches (one big
or multiple `.sav`/`.Rdata` files) this yields the following advantages:

- the data set is often smaller on disk (especially compared to `.sav`
  files)
- meta data can be accessed without loading the actual data
- no need for reshaping, as the different hierarchy levels can be
  accessed independently
- saving working memory: often R struggles with large data sets; with
  `eatGADS` we can choose which variables to load into `R`
- flexible application of value labels and missing codes for analyses in
  `R`

## Inspecting the data base

We can inspect the data base structure with the
[`namesGADS()`](https://beckerbenj.github.io/eatGADS/reference/namesGADS.md)
function. The function returns a named `list`. Every list element
represents a hierarchy level. The corresponding character vector
contains all variable names on this hierarchy level.

``` r
nam <- namesGADS(db_path)
nam
#> $noImp
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
#> [116] "grade_che"    "grade_phy"    "grade_sci"   
#> 
#> $PVs
#> [1] "idstud"    "dimension" "imp"       "value"
```

The example data base contains two hierarchy levels: A student level
(`noImp`) and a plausible value level (`PVs`). On the student level,
each row represents an individual student. On the plausible value level,
each row represents an imputation number of a specific domain of an
individual student.

We can access meta information of the variables in the data set using
the
[`extractMeta()`](https://beckerbenj.github.io/eatGADS/reference/extractMeta.md)
function.

``` r
# Meta data for one variable
extractMeta(db_path, "age")
#>   varName             varLabel format display_width labeled value valLabel
#> 1     age Age of student at T1   F8.2            NA      no    NA     <NA>
#>   missings data_table
#> 1     <NA>      noImp
```

To supply variables names we can also use the named list `nam` extracted
earlier. This way, we can extract all meta information available for a
hierarchy level.

``` r
extractMeta(db_path, nam$PVs)
#>       varName                                       varLabel format
#> 236    idstud                                     Student-ID   F8.0
#> 451    idstud                                     Student-ID   F8.0
#> 452 dimension Achievement dimension (math, reading, science)   <NA>
#> 453 dimension Achievement dimension (math, reading, science)   <NA>
#> 454 dimension Achievement dimension (math, reading, science)   <NA>
#> 455       imp       Number of imputation of plausible values   <NA>
#> 456     value                                Plausible Value   <NA>
#>     display_width labeled value valLabel missings data_table
#> 236            NA      no    NA     <NA>     <NA>      noImp
#> 451            NA      no    NA     <NA>     <NA>        PVs
#> 452            NA     yes     1       ma    valid        PVs
#> 453            NA     yes     2      rea    valid        PVs
#> 454            NA     yes     3      sci    valid        PVs
#> 455            NA      no    NA     <NA>     <NA>        PVs
#> 456            NA      no    NA     <NA>     <NA>        PVs
```

Commonly the most informative columns are `varLabel` (containing
variable labels), `value` (referencing labeled values), `valLabel`
(containing value labels) and `missings` (is a labeled value a missing
value (`"miss"`) or not (`"valid"`)).

``` r
# Meta data for manually chosen multiple variables
extractMeta(db_path, c("idstud", "schtype"))
#>     varName     varLabel format display_width labeled value
#> 236  idstud   Student-ID   F8.0            NA      no    NA
#> 360 schtype School track   F8.0            NA     yes     1
#> 361 schtype School track   F8.0            NA     yes     2
#> 362 schtype School track   F8.0            NA     yes     3
#> 451  idstud   Student-ID   F8.0            NA      no    NA
#>                                      valLabel missings data_table
#> 236                                      <NA>     <NA>      noImp
#> 360                Gymnasium (academic track)    valid      noImp
#> 361                                Realschule    valid      noImp
#> 362 schools with several courses of education    valid      noImp
#> 451                                      <NA>     <NA>        PVs
```

## Extract data from data base

To extract a data set from the data base, we can use the function
[`getGADS()`](https://beckerbenj.github.io/eatGADS/reference/getGADS.md).
If the data base is stored on a server drive,
[`getGADS_fast()`](https://beckerbenj.github.io/eatGADS/reference/getGADS_fast.md)
provides identical functionality but substantially increases the
performance. With the `vSelect` argument we specify our variable
selection. It is important to note that
[`getGADS()`](https://beckerbenj.github.io/eatGADS/reference/getGADS.md)
returns a so called `GADSdat` object. This object type contains complex
meta information (that is for example also available in a `SPSS` data
set), and is therefore not directly usable for data analysis. We can,
however, use the
[`extractMeta()`](https://beckerbenj.github.io/eatGADS/reference/extractMeta.md)
function on it to access the meta data.

``` r
gads1 <- getGADS(filePath = db_path, vSelect = c("idstud", "schtype", "gender"))
class(gads1)
#> [1] "GADSdat" "list"
extractMeta(gads1)
#>     varName     varLabel format display_width labeled value
#> 194  gender       Gender   F8.0            NA     yes     1
#> 195  gender       Gender   F8.0            NA     yes     2
#> 236  idstud   Student-ID   F8.0            NA      no    NA
#> 360 schtype School track   F8.0            NA     yes     1
#> 361 schtype School track   F8.0            NA     yes     2
#> 362 schtype School track   F8.0            NA     yes     3
#>                                      valLabel missings
#> 194                                    Female    valid
#> 195                                      Male    valid
#> 236                                      <NA>     <NA>
#> 360                Gymnasium (academic track)    valid
#> 361                                Realschule    valid
#> 362 schools with several courses of education    valid
```

## Extract data from `GADSdat`

If we want to use the data for analyses in `R` we have to extract it
from the `GADSdat` object via the function
[`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md).
In doing so, we have to make two important decisions: (a) how should
values marked as missing values be treated (`convertMiss`)? And (b) how
should labeled values in general be treated (`labels2character`,
`labels2factor`, `labels2ordered`, and `dropPartialLabels`)?

Per default, all missing tags are applied, meaning all values tagged as
missing are recoded to `NA` (`convertMiss == TRUE`). Furthermore, per
default, all value labels are dropped (`labels2character = NULL`,
`labels2factor = NULL`, `labels2ordered = NULL`). If for specific
variables, value labels should be applied and the resulting variable
should be a character variable, this can specified via, for example,
setting `labels2character = c("var1", "var2")`.

``` r
## leave all labeled variables as numeric, convert missings to NA
dat1 <- extractData2(gads1)
head(dat1)
#>   idstud schtype gender
#> 1      1       2      1
#> 2      2       3      1
#> 3      3       1      2
#> 4      4       3      2
#> 5      5       2      1
#> 6      6       3      1

## convert selected labeled variable(s) to character, convert missings to NA
dat2 <- extractData2(gads1, labels2character = c("schtype"))
head(dat2)
#>   idstud                                   schtype gender
#> 1      1                                Realschule      1
#> 2      2 schools with several courses of education      1
#> 3      3                Gymnasium (academic track)      2
#> 4      4 schools with several courses of education      2
#> 5      5                                Realschule      1
#> 6      6 schools with several courses of education      1

## convert all labeled variables to character, convert missings to NA
dat3 <- extractData2(gads1, labels2character = namesGADS(gads1))
head(dat3)
#>   idstud                                   schtype gender
#> 1      1                                Realschule Female
#> 2      2 schools with several courses of education Female
#> 3      3                Gymnasium (academic track)   Male
#> 4      4 schools with several courses of education   Male
#> 5      5                                Realschule Female
#> 6      6 schools with several courses of education Female
```

In general, we recommend leaving labeled variables as numeric and
converting values with missing codes to `NA`. If required, value labels
can always be accessed via using
[`extractMeta()`](https://beckerbenj.github.io/eatGADS/reference/extractMeta.md)
on the `GADSdat` object or the data base.

## Selecting different hierarchy levels

An important feature of `eatGADS` relational data bases are that data
sets are automatically returned on the correct hierarchy level. For an
overview of different data structures, see [“Tidy
Data”](https://doi.org/10.18637/jss.v059.i10) or [this article
explaining long and wide format using repeated
measures](https://www.theanalysisfactor.com/wide-and-long-data/). In
educational large-scale assessments, data usually contain multiple
imputations or plausible values. Packages that enable us analyzing these
types of data (like [`eatRep`](https://github.com/weirichs/eatRep))
often require these data in the long format.

The function
[`getGADS()`](https://beckerbenj.github.io/eatGADS/reference/getGADS.md)
extracts data automatically in the appropriate structure, depending on
our variable selection. If we select only variables from the student
level, the data returned is on the student level. Each student is
represented in a single row.

``` r
gads1 <- getGADS(db_path, vSelect = c("schtype", "g8g9"))
dat1 <- extractData2(gads1)
dim(dat1)
#> [1] 500   3
head(dat1)
#>   idstud schtype g8g9
#> 1      1       2   NA
#> 2      2       3   NA
#> 3      3       1    1
#> 4      4       3   NA
#> 5      5       2   NA
#> 6      6       3   NA
```

If additionally variables from the plausible Value data table are
extracted, the returned data structure changes. In the `PVs` data table,
data is stored on the “student x dimension x plausible value number”
level. The returned data has exactly this structure.

``` r
gads2 <- getGADS(db_path, vSelect = c("schtype", "value"))
dat2 <- extractData2(gads2)
dim(dat2)
#> [1] 7500    5
head(dat2)
#>   idstud schtype dimension imp       value
#> 1      1       2         1   1  0.15372011
#> 2      1       2         2   1  0.43914365
#> 3      1       2         3   1  0.13177617
#> 4      1       2         1   2 -0.04119330
#> 5      1       2         2   2  0.01991714
#> 6      1       2         3   2  0.67830064
```

These two examples highlight another feature of
[`getGADS()`](https://beckerbenj.github.io/eatGADS/reference/getGADS.md):
Only variables of substantial interest have to be selected for
extraction. The correct ID variables are added automatically.

## Trend data bases

In educational large-scale assessments, a common challenge is reporting
longitudinal developments (*trends*). `getTrendGADS` allows extracting
data from multiple data bases with identical variables in it.

``` r
trend_path1 <- system.file("extdata", "trend_gads_2020.db", package = "eatGADS")
trend_path2 <- system.file("extdata", "trend_gads_2015.db", package = "eatGADS")
trend_path3 <- system.file("extdata", "trend_gads_2010.db", package = "eatGADS")
```

`eatGADS` comes with three small trend data bases which can be used for
illustrative purposes.

``` r
gads_trend <- getTrendGADS(filePaths = c(trend_path1, trend_path2, trend_path3), 
                           vSelect = c("idstud", "dimension", "score"), 
                           years = c(2020, 2015, 2010), fast = FALSE)
#>  -----  Loading GADS 2020 ----- 
#>  -----  Loading GADS 2015 ----- 
#>  -----  Loading GADS 2010 -----
dat_trend <- extractData2(gads_trend)
head(dat_trend)
#>   idstud dimension imp    score year
#> 1     61         1   2 434.1060 2020
#> 2     61         2   2 434.1060 2020
#> 3     62         1   1 426.8199 2020
#> 4     62         2   1 426.8199 2020
#> 5     63         1   2 427.7327 2020
#> 6     63         2   2 427.7327 2020
```
