# Check Lengths of Labels.

Check if the value or variable labels of a `GADSdat` comply with the
length limits imposed by `SPSS` or `Stata`.

## Usage

``` r
checkValLabels(
  GADSdat,
  charLimits = c("SPSS", "Stata"),
  vars = namesGADS(GADSdat),
  printLength = 40
)

checkVarLabels(
  GADSdat,
  charLimits = c("SPSS", "Stata"),
  vars = namesGADS(GADSdat),
  printLength = 40
)
```

## Arguments

- GADSdat:

  A `GADSdat` object.

- charLimits:

  Character vector of the program(s) against whose limit(s) the labels
  should be checked.

- vars:

  Optional character vector of the variables whose value labels should
  be checked. By default, all value labels will be checked.

- printLength:

  Single numeric value. The first n = `printLength` characters of each
  long label will be reported. Alternatively, specify `NULL` to
  deactivate the truncation and have the function report the full labels
  (not recommended; also see details).

## Value

A `data.frame`, reporting every (truncated) long `varLabel`/`valLabel`,
their respective `length` in the relevant `unit` and the `varName` in
which they occur. For `checkValLabels`, the labeled `value`, as well as
whether that value actually occurs in the data (`empty`), is also
reported.

## Details

If more than one program name is given in `charLimits`, the most
restrictive limit will be applied. For details about program-specific
limits, see
[program_limits](https://beckerbenj.github.io/eatGADS/reference/program_limits.md).

Please note that setting `printLength` to `NULL` (and thereby
deactivating label truncation) might not actually result in the printing
of the full length of the exceedingly long labels if you are using
RStudio. The program's own limits on the number of characters printed to
the console may still apply (see [Stack
Overflow](https://stackoverflow.com/questions/36800475/avoid-string-printed-to-console-getting-truncated-in-rstudio)).

## Functions

- `checkValLabels()`: Check value labels for length limits.

- `checkVarLabels()`: Check variable labels for length limits.

## Examples

``` r
# check value labels
pisa2 <- pisa
pisa2$labels[4, "valLabel"] <- paste0(rep("abcdefg", 4300), collapse = "")
eatGADS:::checkValLabels(pisa2)
#>   varName value                                    valLabel length unit empty
#> 5 schtype     1 abcdefgabcdefgabcdefgabcdefgabcdefgabcde...  30100 byte FALSE

# check variable labels
pisa2$labels[1, "varLabel"] <- paste0(rep("abcdefg", 12), collapse = "")
eatGADS:::checkVarLabels(pisa2)
#>     varName                                    varLabel length unit
#> 1    idstud abcdefgabcdefgabcdefgabcdefgabcdefgabcde...     84 char
#> 2 attitud_a Attitude towards School - Does Little to...     81 char
```
