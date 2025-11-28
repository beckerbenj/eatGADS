# List of program specific limits

A list of two matrices, detailing different limits for datasets specific
to `SPSS` and `Stata`.

## Usage

``` r
program_limits
```

## Format

A data.frame.

## Details

`SPSS` and `Stata` impose different limits to different aspects of their
datasets:

- `varNames`: length of variable names

- `varLabels`: length of variable labels

- `valLabels`: length of value labels

- `stringvars`: length of strings in character variables

- `nrows`: number of observations

- `ncols`: number of variables

While `SPSS` has only one set of limits (disregarding legacy limits for
older versions), `Stata`, employs different limits for different product
versions \[1\]. Here, `SPSS` implies `SPSS 30`, and `Stata` implies
`Stata 19/SE`. Limits of `Stata 19/BE` and `Stata 19/MP` are implemented
as additional options for
[getProgramLimit](https://beckerbenj.github.io/eatGADS/reference/getProgramLimit.md).

## References

\[1\] [Stata: Comparison of
limits](https://www.stata.com/products/comparison-of-limits/)
