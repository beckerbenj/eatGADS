# Program specific limits to dataset components

Different programs impose different limits to different components of
their datasets. Additionally, limits may vary between software versions.
This primarily applies to `Stata`'s product tiers, but also to (very)
old `SPSS` versions. `eatGADS` offers a number of check functions -
chiefly
[`check4SPSS`](https://beckerbenj.github.io/eatGADS/reference/check4SPSS.md)
and
[`check4Stata`](https://beckerbenj.github.io/eatGADS/reference/check4Stata.md)
as wrappers - to ensure a `GADSdat` complies with these limits, and can
be exported into an
[SPSS](https://beckerbenj.github.io/eatGADS/reference/write_spss.md) or
[Stata](https://beckerbenj.github.io/eatGADS/reference/write_spss.md)
file. Use
[`getProgramLimit`](https://beckerbenj.github.io/eatGADS/reference/getProgramLimit.md)
for a more convenient interface for obtaining specific limits.

## Usage

``` r
program_limits
```

## Format

A data.frame listing relevant limits (see details) imposed to datasets
by `SPSS` and `Stata`.

## Details

While datasets have several components and characteristics, the
following were deemed the most important and their limits implemented in
this package's checks:

- `varNames`: length of variable names

- `varLabels`: length of variable labels

- `valLabels`: length of value labels

- `stringvars`: length of strings in character variables

- `nrows`: number of observations

- `ncols`: number of variables

While `SPSS` has only one set of limits (disregarding legacy limits for
older versions), `Stata` employs different limits for different product
versions \[1\]. Within this package, `"SPSS"` always implies `SPSS 30`,
and `"Stata"` implies `Stata 19/SE`. Limits of `Stata 19/BE` and
`Stata 19/MP` are implemented as additional options. However, no
additional versions of `SPSS` have been implemented yet.

## References

\[1\] [Stata: Comparison of
limits](https://www.stata.com/products/comparison-of-limits/)
