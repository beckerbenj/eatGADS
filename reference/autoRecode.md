# Auto recode a variable in a `GADSdat`.

Auto recode a variable in a `GADSdat`, mirroring the core functionality
provided by `AUTORECODE` in `SPSS`. A lookup table containing the
respective recode pairs can be applied and/or saved.

## Usage

``` r
autoRecode(
  GADSdat,
  var,
  var_suffix = "",
  label_suffix = "",
  csv_path = NULL,
  template = NULL
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- var:

  Character string of the variable name which should be recoded.

- var_suffix:

  Variable suffix for the newly created `GADSdat`. If an empty
  character, the existing variables are overwritten.

- label_suffix:

  Suffix added to variable label for the newly created variable in the
  `GADSdat`.

- csv_path:

  Path for the `.csv` file for the lookup table.

- template:

  Existing lookup table.

## Value

Returns a `GADSdat` object.

## Details

Existing values are replaced with sequential numbers, and all existing
value-level metadata (`valLabel` and `missings`) are dropped. This can
be useful to remove confidential information from ID variables. If the
original (`character`) values are to be preserved as `valLabel`s,
[multiChar2fac](https://beckerbenj.github.io/eatGADS/reference/multiChar2fac.md)
should be used instead.

An existing `template` may be used to ensure that identical original
values are recoded as the same new values. The lookup table used to
recode `var` may also be saved as a `.csv` file, e.g. to be used as a
`template` later. If both an existing `template` is used and the lookup
table is saved, the resulting lookup table will contain the existing
recodes and additional recode pairs required for the data, if any were
needed.

## Examples

``` r
gads <- import_DF(data.frame(v1 = letters))

# auto recode without saving lookup table
gads2 <- autoRecode(gads, var = "v1", var_suffix = "_num")

# auto recode with saving lookup table
f <- tempfile(fileext = ".csv")
gads2 <- autoRecode(gads, var = "v1", var_suffix = "_num", csv_path = f)

# auto recode with applying and expanding a lookup table
gads3 <- import_DF(data.frame(v2 = c(letters[1:3], "aa")))
gads3 <- autoRecode(gads3, var = "v2", csv_path = f,
                    template = read.csv(f))
#> Warning: For variable v2 the following values are in the lookup table but not in the data: d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
```
