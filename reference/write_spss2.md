# Write a `GADSdat` object to `txt` and `SPSS` syntax

Write a `GADSdat` object to a text file (`txt`) and an accompanying
`SPSS` syntax file containing all meta information (e.g. value and
variable labels).

## Usage

``` r
write_spss2(
  GADSdat,
  txtPath,
  spsPath = NULL,
  savPath = NULL,
  dec = ".",
  fileEncoding = "UTF-8",
  chkFormat = TRUE,
  ...
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- txtPath:

  Path of `.txt` file to write, including file name and ending `.txt`.
  No default.

- spsPath:

  Path of `.sps` file to write, including file name and ending `.sps`.
  Default Path is `txtPath`.

- savPath:

  Path of `.sav` file to write, including file name and ending `.sav`.
  Default Path is `spsPath`.

- dec:

  Decimal delimiter for your SPSS version. Other values for `dec` than
  `","` or `"."` are not implemented yet.

- fileEncoding:

  Data file encoding for SPSS. Default is `"UTF-8"`.

- chkFormat:

  Whether format checks via `checkFormat` should be performed.

- ...:

  Arguments to pass to `checkFormat` in particular `changeFormat=FALSE`
  if needed.

## Value

Writes a `txt` and an `sav` file to disc, returns nothing.

## Details

This function is based on `eatPreps` `writeSpss` function and is
currently under development.

## Examples

``` r
# write to spss
tmp_txt <- tempfile(fileext = ".txt")
write_spss2(pisa, txtPath = tmp_txt)
#> Variable ma_pv1 has more decimals than SPSS allows (18) and will be rounded to 16 decimal places.
#> Variable ma_pv2 has more decimals than SPSS allows (18) and will be rounded to 16 decimal places.
#> Variable ma_pv3 has more decimals than SPSS allows (18) and will be rounded to 16 decimal places.
#> Variable ma_pv4 has more decimals than SPSS allows (18) and will be rounded to 16 decimal places.
#> Variable ma_pv5 has more decimals than SPSS allows (18) and will be rounded to 16 decimal places.
#> Variable rea_pv1 has more decimals than SPSS allows (17) and will be rounded to 16 decimal places.
#> Variable rea_pv2 has more decimals than SPSS allows (17) and will be rounded to 16 decimal places.
#> Variable rea_pv3 has more decimals than SPSS allows (17) and will be rounded to 16 decimal places.
#> Variable rea_pv4 has more decimals than SPSS allows (18) and will be rounded to 16 decimal places.
#> Variable rea_pv5 has more decimals than SPSS allows (17) and will be rounded to 16 decimal places.
#> Variable sci_pv1 has more decimals than SPSS allows (17) and will be rounded to 16 decimal places.
#> Variable sci_pv2 has more decimals than SPSS allows (17) and will be rounded to 16 decimal places.
#> Variable sci_pv3 has more decimals than SPSS allows (17) and will be rounded to 16 decimal places.
#> Variable sci_pv4 has more decimals than SPSS allows (17) and will be rounded to 16 decimal places.
#> Variable sci_pv5 has more decimals than SPSS allows (17) and will be rounded to 16 decimal places.
#> Format of Variable idstud will be changed from F8.0 to F3
#> Format of Variable idschool will be changed from F8.0 to F3
#> Format of Variable idclass will be changed from F8.0 to F3
#> Format of Variable schtype will be changed from F8.0 to F1
#> Format of Variable sameteach will be changed from F8.0 to F1
#> Format of Variable g8g9 will be changed from F8.0 to F1
#> Format of Variable ganztag will be changed from F8.0 to F1
#> Format of Variable classsize will be changed from F8.0 to F2
#> Format of Variable repeated will be changed from F8.0 to F1
#> Format of Variable gender will be changed from F8.0 to F1
#> Format of Variable age will be changed from F8.2 to F5.2
#> Format of Variable language will be changed from F8.0 to F2
#> Format of Variable migration will be changed from F8.0 to F1
#> Format of Variable hisced will be changed from F8.0 to F1
#> Format of Variable hisei will be changed from F8.2 to F2
#> Format of Variable homepos will be changed from F8.2 to F4.2
#> Format of Variable books will be changed from F8.0 to F1
#> Format of Variable pared will be changed from F8.2 to F2
#> Format of Variable computer_age will be changed from F8.0 to F1
#> Format of Variable internet_age will be changed from F8.0 to F1
#> Format of Variable int_use_a will be changed from F8.0 to F1
#> Format of Variable int_use_b will be changed from F8.0 to F1
#> Format of Variable truancy_a will be changed from F8.0 to F1
#> Format of Variable truancy_b will be changed from F8.0 to F1
#> Format of Variable truancy_c will be changed from F8.0 to F1
#> Format of Variable int_a will be changed from F8.0 to F1
#> Format of Variable int_b will be changed from F8.0 to F1
#> Format of Variable int_c will be changed from F8.0 to F1
#> Format of Variable int_d will be changed from F8.0 to F1
#> Format of Variable instmot_a will be changed from F8.0 to F1
#> Format of Variable instmot_b will be changed from F8.0 to F1
#> Format of Variable instmot_c will be changed from F8.0 to F1
#> Format of Variable instmot_d will be changed from F8.0 to F1
#> Format of Variable norms_a will be changed from F8.0 to F1
#> Format of Variable norms_b will be changed from F8.0 to F1
#> Format of Variable norms_c will be changed from F8.0 to F1
#> Format of Variable norms_d will be changed from F8.0 to F1
#> Format of Variable norms_e will be changed from F8.0 to F1
#> Format of Variable norms_f will be changed from F8.0 to F1
#> Format of Variable anxiety_a will be changed from F8.0 to F1
#> Format of Variable anxiety_b will be changed from F8.0 to F1
#> Format of Variable anxiety_c will be changed from F8.0 to F1
#> Format of Variable anxiety_d will be changed from F8.0 to F1
#> Format of Variable anxiety_e will be changed from F8.0 to F1
#> Format of Variable selfcon_a will be changed from F8.0 to F1
#> Format of Variable selfcon_b will be changed from F8.0 to F1
#> Format of Variable selfcon_c will be changed from F8.0 to F1
#> Format of Variable selfcon_d will be changed from F8.0 to F1
#> Format of Variable selfcon_e will be changed from F8.0 to F1
#> Format of Variable worketh_a will be changed from F8.0 to F1
#> Format of Variable worketh_b will be changed from F8.0 to F1
#> Format of Variable worketh_c will be changed from F8.0 to F1
#> Format of Variable worketh_d will be changed from F8.0 to F1
#> Format of Variable worketh_e will be changed from F8.0 to F1
#> Format of Variable worketh_f will be changed from F8.0 to F1
#> Format of Variable worketh_g will be changed from F8.0 to F1
#> Format of Variable worketh_h will be changed from F8.0 to F1
#> Format of Variable worketh_i will be changed from F8.0 to F1
#> Format of Variable intent_a will be changed from F8.0 to F1
#> Format of Variable intent_b will be changed from F8.0 to F1
#> Format of Variable intent_c will be changed from F8.0 to F1
#> Format of Variable intent_d will be changed from F8.0 to F1
#> Format of Variable intent_e will be changed from F8.0 to F1
#> Format of Variable behav_a will be changed from F8.0 to F1
#> Format of Variable behav_b will be changed from F8.0 to F1
#> Format of Variable behav_c will be changed from F8.0 to F1
#> Format of Variable behav_d will be changed from F8.0 to F1
#> Format of Variable behav_e will be changed from F8.0 to F1
#> Format of Variable behav_f will be changed from F8.0 to F1
#> Format of Variable behav_g will be changed from F8.0 to F1
#> Format of Variable behav_h will be changed from F8.0 to F1
#> Format of Variable teach_a will be changed from F8.0 to F1
#> Format of Variable teach_b will be changed from F8.0 to F1
#> Format of Variable teach_c will be changed from F8.0 to F1
#> Format of Variable teach_d will be changed from F8.0 to F1
#> Format of Variable teach_e will be changed from F8.0 to F1
#> Format of Variable cognact_a will be changed from F8.0 to F1
#> Format of Variable cognact_b will be changed from F8.0 to F1
#> Format of Variable cognact_c will be changed from F8.0 to F1
#> Format of Variable cognact_d will be changed from F8.0 to F1
#> Format of Variable cognact_e will be changed from F8.0 to F1
#> Format of Variable cognact_f will be changed from F8.0 to F1
#> Format of Variable cognact_g will be changed from F8.0 to F1
#> Format of Variable cognact_h will be changed from F8.0 to F1
#> Format of Variable cognact_i will be changed from F8.0 to F1
#> Format of Variable discpline_a will be changed from F8.0 to F1
#> Format of Variable discpline_b will be changed from F8.0 to F1
#> Format of Variable discpline_c will be changed from F8.0 to F1
#> Format of Variable discpline_d will be changed from F8.0 to F1
#> Format of Variable discpline_e will be changed from F8.0 to F1
#> Format of Variable relation_a will be changed from F8.0 to F1
#> Format of Variable relation_b will be changed from F8.0 to F1
#> Format of Variable relation_c will be changed from F8.0 to F1
#> Format of Variable relation_d will be changed from F8.0 to F1
#> Format of Variable relation_e will be changed from F8.0 to F1
#> Format of Variable belong_a will be changed from F8.0 to F1
#> Format of Variable belong_b will be changed from F8.0 to F1
#> Format of Variable belong_c will be changed from F8.0 to F1
#> Format of Variable belong_d will be changed from F8.0 to F1
#> Format of Variable belong_e will be changed from F8.0 to F1
#> Format of Variable belong_f will be changed from F8.0 to F1
#> Format of Variable belong_g will be changed from F8.0 to F1
#> Format of Variable belong_h will be changed from F8.0 to F1
#> Format of Variable belong_i will be changed from F8.0 to F1
#> Format of Variable attitud_a will be changed from F8.0 to F1
#> Format of Variable attitud_b will be changed from F8.0 to F1
#> Format of Variable attitud_c will be changed from F8.0 to F1
#> Format of Variable attitud_d will be changed from F8.0 to F1
#> Format of Variable attitud_e will be changed from F8.0 to F1
#> Format of Variable attitud_f will be changed from F8.0 to F1
#> Format of Variable attitud_g will be changed from F8.0 to F1
#> Format of Variable attitud_h will be changed from F8.0 to F1
#> Format of Variable grade_de will be changed from F8.0 to F1
#> Format of Variable grade_ma will be changed from F8.0 to F1
#> Format of Variable grade_bio will be changed from F8.0 to F1
#> Format of Variable grade_che will be changed from F8.0 to F1
#> Format of Variable grade_phy will be changed from F8.0 to F1
#> Format of Variable grade_sci will be changed from F8.0 to F1
#> Format of Variable ma_pv1 will be changed from F8.2 to F18.16
#> Format of Variable ma_pv2 will be changed from F8.2 to F18.16
#> Format of Variable ma_pv3 will be changed from F8.2 to F18.16
#> Format of Variable ma_pv4 will be changed from F8.2 to F18.16
#> Format of Variable ma_pv5 will be changed from F8.2 to F18.16
#> Format of Variable rea_pv1 will be changed from F8.2 to F18.16
#> Format of Variable rea_pv2 will be changed from F8.2 to F18.16
#> Format of Variable rea_pv3 will be changed from F8.2 to F18.16
#> Format of Variable rea_pv4 will be changed from F8.2 to F18.16
#> Format of Variable rea_pv5 will be changed from F8.2 to F18.16
#> Format of Variable sci_pv1 will be changed from F8.2 to F18.16
#> Format of Variable sci_pv2 will be changed from F8.2 to F18.16
#> Format of Variable sci_pv3 will be changed from F8.2 to F18.16
#> Format of Variable sci_pv4 will be changed from F8.2 to F18.16
#> Format of Variable sci_pv5 will be changed from F8.2 to F18.16
```
