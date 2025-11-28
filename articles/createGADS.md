# \`createGADS\`: Creating a relational data base

In the context of educational large-scale assessments (but also in other
contexts) we frequently encounter data sets which have an hierarchical
structure. In educational large-scale assessments these can, for
example, be pupils nested in schools. Additional, hidden nested
structures occur, if missing data are treated with multiple imputations
or person parameters are estimated using plausible values. In these
cases it is inefficient to store all the data in one rectangular data
set. In other data science applications the use of relational data bases
is a widespread measure to tackle this challenge.

`eatGADS` supports creating such relational data bases (based on the
open source software `SQLite3` and the `R` package `eatDB`) while
maintaining its meta data and providing a very user friendly interface
for data users who are unfamiliar with relational data bases. In doing
so, it allows the handling of large data sets even on limited hardware
settings. Furthermore, this approach allows the extraction of data from
different hierarchy levels, which means that data has to be reshaped
very rarely.

This vignette illustrates how a relational `eatGADS` data base can be
created from a rectangular `SPSS` (`.sav`) data file. For illustrative
purposes we use a small example data set from the campus files of the
German PISA Plus assessment. The complete campus files and the original
data set can be accessed
[here](https://www.iqb.hu-berlin.de/fdz/Datenzugang/CF-Antrag/AntragsformularCF)
and [here](https://www.iqb.hu-berlin.de/fdz/Datenzugang/SUF-Antrag).

## Importing data

``` r
library(eatGADS)
```

We can import an `.sav` (or an compressed `.zsav`) data set via the
[`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
function. Checks on variable names for `SQLite3` compliance are
performed automatically. Changes to the variable names are reported to
the console.

``` r
sav_path <- system.file("extdata", "pisa.zsav", package = "eatGADS")
dat <- import_spss(sav_path)
```

The next steps depend on the data structure: If the different hierarchy
levels are saved in different source data sets (e.g. different `.sav`
files) the next section can be skipped. However, sometimes data from
different hierarchy levels is saved in one data file. Then, splitting
and reshaping becomes necessary.

## Splitting and Reshaping

In this case, we want to split the imported `GADSdat` object into its
hierarchy levels (in our example: background data on level 1 and imputed
competence data on level 2). This can be achieved by the
[`splitGADS()`](https://beckerbenj.github.io/eatGADS/reference/splitGADS.md)
function. We specify the hierarchical structure as a `list`. After this,
we can extract separate `GADSdat` objects by name via the
`extractGADS()` function. These objects can then be used for reshaping.

For reasons of simplicity, the example only contains two hierarchy
levels. In practice, often more hierarchy levels are present. Splitting
can be performed into as many hierarchy levels as desired. The reshaping
has to be performed for each hierarchy level separately.

``` r
pvs <- grep("pv", namesGADS(dat), value = T)
splitted_gads <- splitGADS(dat, nameList = list(noImp = namesGADS(dat)[!namesGADS(dat) %in% pvs],
                    PVs = c("idstud", pvs)))
# new Structure
namesGADS(splitted_gads)
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
#>  [1] "idstud"  "ma_pv1"  "ma_pv2"  "ma_pv3"  "ma_pv4"  "ma_pv5"  "rea_pv1"
#>  [8] "rea_pv2" "rea_pv3" "rea_pv4" "rea_pv5" "sci_pv1" "sci_pv2" "sci_pv3"
#> [15] "sci_pv4" "sci_pv5"

# Extract GADSdat objects
noImp_gads <- extractGADSdat(splitted_gads, "noImp")
pvs_gads <- extractGADSdat(splitted_gads, "PVs")
```

For reshaping data we highly recommend the `R` package `tidyr`. Its
performance might be less optimized than for example the `data.table`
package, however it is very intuitive and user friendly. For our example
data set we need to reshape the `PVs` from wide to long format and then
separate the resulting column into two columns, containing the
`dimension` and imputation number (`imp`) (Note: This results in a data
set in which different dimensions for a single student are stored in
separate rows, not columns). For this, we directly access the data in
the `GADSdat` object via `pvs_gads$dat`. The reshaping is performed by
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html).
[`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html)
is used to separate our two additional identifier columns (`dimension`
and `imp`). Finally, we clean the `imp` column and transform it to
numeric.

``` r
# Extract raw data from pv gads
pvs_wide <- pvs_gads$dat

# Wide format
head(pvs_wide)
#>   idstud     ma_pv1     ma_pv2     ma_pv3      ma_pv4     ma_pv5    rea_pv1
#> 1      1  0.1537201 -0.0411933  0.5702895  0.01687233  0.3003968  0.4391437
#> 2      2 -0.3690980 -0.1201779 -0.2164011 -0.64099562 -0.3626861 -0.3471025
#> 3      3  1.7042239  2.2205527  1.7162633  2.78119427  2.6928097  0.8667544
#> 4      4  0.3490264  0.6069737  1.0037767  0.67002173  0.8012499 -0.7661811
#> 5      5 -0.6379547 -0.8142668 -0.6153099 -0.38015661 -0.1363339  0.1145925
#> 6      6 -1.5558856 -2.0435904 -0.7931236 -1.26866066 -1.1869012 -1.0732799
#>       rea_pv2     rea_pv3     rea_pv4    rea_pv5    sci_pv1    sci_pv2
#> 1  0.01991714  1.42848870 -0.06243637  0.8371030  0.1317762  0.6783006
#> 2  0.09553654  0.49335276  0.10951613  0.6657507 -0.8650453 -0.3834589
#> 3  0.61768689  1.17497378  1.12938438  1.3001419  1.1035166  1.2730882
#> 4  0.80961068  0.09573558 -0.23817788  0.2853083 -0.3049963  0.2290473
#> 5 -0.08762244  0.06418227  0.57376133 -0.5326255 -0.8032184 -0.6878142
#> 6 -1.18496034 -0.67843740 -0.06669544 -0.5332718 -0.9191711 -1.6379850
#>       sci_pv3     sci_pv4    sci_pv5
#> 1  1.46203909  0.61406429  0.4807234
#> 2 -0.54372393 -1.00303484 -0.8101605
#> 3  1.51685344  1.61485031  1.6091542
#> 4  0.18340247 -0.06804704  0.2677832
#> 5 -0.03322359  0.43998031  0.3998337
#> 6 -0.80060130 -0.43433496 -1.3110661

pvs_long1 <- tidyr::pivot_longer(pvs_wide, cols = all_of(pvs))
pvs_long2 <- tidyr::separate(pvs_long1, col = "name", sep = "_", into = c("dimension", "imp"))
pvs_long2$imp <- as.numeric(gsub("pv", "", pvs_long2$imp))

# Finale long format
head(as.data.frame(pvs_long2))
#>   idstud dimension imp       value
#> 1      1        ma   1  0.15372011
#> 2      1        ma   2 -0.04119330
#> 3      1        ma   3  0.57028949
#> 4      1        ma   4  0.01687233
#> 5      1        ma   5  0.30039680
#> 6      1       rea   1  0.43914365
```

## Handling meta data

After reshaping we adapt the meta data in our initial `GADSdat` object
via
[`updateMeta()`](https://beckerbenj.github.io/eatGADS/reference/updateMeta.md).
This is necessary, as variables have been removed from the data set
(e.g. `"ma_pv1"` etc.) and new variables have replaced them (`"value"`,
`"dimension"`, `"imp"`). Now we have to add some variable labels, as
most of the old variable labels got lost due to the reshaping. For an
extensive tutorial see the vignette [Handling Meta
Data](https://beckerbenj.github.io/eatGADS/articles/import_raw.md).

``` r
pvs_gads2 <- updateMeta(pvs_gads, newDat = as.data.frame(pvs_long2))
#> Removing the following rows from meta data: ma_pv1, ma_pv2, ma_pv3, ma_pv4, ma_pv5, rea_pv1, rea_pv2, rea_pv3, rea_pv4, rea_pv5, sci_pv1, sci_pv2, sci_pv3, sci_pv4, sci_pv5
#> Adding meta data for the following variables: dimension, imp, value
extractMeta(pvs_gads2)
#>             varName   varLabel format display_width labeled value valLabel
#> 1            idstud Student-ID   F8.0            NA      no    NA     <NA>
#> dimension dimension       <NA>   <NA>            NA      no    NA     <NA>
#> imp             imp       <NA>   <NA>            NA      no    NA     <NA>
#> value         value       <NA>   <NA>            NA      no    NA     <NA>
#>           missings
#> 1             <NA>
#> dimension     <NA>
#> imp           <NA>
#> value         <NA>

# 
pvs_gads2 <- changeVarLabels(pvs_gads2, varName = c("dimension", "imp", "value"),
                varLabel = c("Achievement dimension (math, reading, science)",
                             "Number of imputation of plausible values",
                             "Plausible Value"))
extractMeta(pvs_gads2)
#>             varName                                       varLabel format
#> 1            idstud                                     Student-ID   F8.0
#> dimension dimension Achievement dimension (math, reading, science)   <NA>
#> imp             imp       Number of imputation of plausible values   <NA>
#> value         value                                Plausible Value   <NA>
#>           display_width labeled value valLabel missings
#> 1                    NA      no    NA     <NA>     <NA>
#> dimension            NA      no    NA     <NA>     <NA>
#> imp                  NA      no    NA     <NA>     <NA>
#> value                NA      no    NA     <NA>     <NA>
```

## Preparing and Creating the data base

For the creation of a relational data base we recreate the initial
hierarchical structure via
[`mergeLabels()`](https://beckerbenj.github.io/eatGADS/reference/mergeLabels.md)
(which performs the reverse action as `extractGADS()`). Furthermore, we
create two lists, a primary key list (`pkList`) and a foreign key list
(`fkList`). Primary keys are the variables that uniquely identify each
row within every hierarchy level. Foreign keys are the variables that
allow merging between different hierarchy levels. In the list of foreign
keys we also have to specify another hierarchy level, to which each
hierarchy level connects. An exception is the lowest hierarchy levels,
which serves as a basis.

By setting up the order and the foreign keys of the data base we specify
how the data is merged together when we extract data from it. In
contrast to conventional relational data bases, `eatGADS` data bases are
less flexible: The package does not support modifying the data base
after its creation or extracting data from it with different merges than
specified when it was set up.

``` r
merged_gads <- mergeLabels(noImp = noImp_gads, PVs = pvs_gads2)

pkList <- list(noImp = "idstud",
               PVs = c("idstud", "imp", "dimension"))
fkList <- list(noImp = list(References = NULL, Keys = NULL),
               PVs = list(References = "noImp", Keys = "idstud"))
```

Finally, we create the relational data base on disc via the
[`createGADS()`](https://beckerbenj.github.io/eatGADS/reference/createGADS.md)
function.

``` r
temp_path <- paste0(tempfile(), ".db")

createGADS(merged_gads, pkList = pkList, fkList = fkList,
           filePath = temp_path)
#> NULL
```

For a detailed tutorial on how to use a relational `eatGADS` data base
see the vignette [`getGADS`: Using a relational eatGADS data
base](https://beckerbenj.github.io/eatGADS/articles/getGADS.md).
