# Variables names of a GADS.

Variables names of a `GADSdat` object, a `all_GADSdat` object or a
`eatGADS` data base.

## Usage

``` r
namesGADS(GADS)
```

## Arguments

- GADS:

  A `GADSdat` object, a `all_GADSdat` or the path to an existing
  `eatGADS` data base.

## Value

Returns a character vector or a named list of character vectors.

## Details

If the function is applied to a `GADSdat` object, a character vector
with all variable names is returned. If the function is applied to a
`all_GADSdat` object or to the path of a `eatGADS` data base, a named
list is returned. Each list entry represents a data table in the object.

## Examples

``` r
# Extract variable names from data base
db_path <- system.file("extdata", "pisa.db", package = "eatGADS")
namesGADS(db_path)
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
#> 

# Extract variable names  from loaded/imported GADS
namesGADS(pisa)
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
```
