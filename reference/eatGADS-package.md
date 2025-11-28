# eatGADS: Data management of hierarchical SPSS files via R and SQLite

The `eatGADS` package provides various functionality, mainly: importing
data, data and meta data cleaning, creating a fixed form `SQLite` data
base and using the `SQLite` data base.

## Importing data

`SPSS` data (`.sav`) can be imported via
[`import_spss`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md).
Further import functions exist as well:
[`import_stata`](https://beckerbenj.github.io/eatGADS/reference/import_stata.md)
for importing `Stata` data (`.dta`),
[`import_DF`](https://beckerbenj.github.io/eatGADS/reference/import_DF.md)
for importing `R` `data.frames`,
[`import_RDS`](https://beckerbenj.github.io/eatGADS/reference/import_RDS.md)
for importing `R` `data.frames` saved as `.RDS` files, and
[`import_raw`](https://beckerbenj.github.io/eatGADS/reference/import_raw.md)
as well as
[`import_raw2`](https://beckerbenj.github.io/eatGADS/reference/import_raw2.md)
for importing data from raw data and meta data files.

## Data and meta data cleaning

Data cleaning functions include functions for recoding data (e.g.,
[`recodeGADS`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md))
or re-ordering variables (e.g.,
[`relocateVariable`](https://beckerbenj.github.io/eatGADS/reference/relocateVariable.md)).
Meta data cleaning functions include functions for changing variables
labels (e.g.,
[`changeVarLabels`](https://beckerbenj.github.io/eatGADS/reference/changeVarLabels.md)),
changing value labels
[`changeValLabels`](https://beckerbenj.github.io/eatGADS/reference/changeValLabels.md)
or modifying missings tags
[`changeMissings`](https://beckerbenj.github.io/eatGADS/reference/changeMissings.md).

## Creating a GADS data base

Hierarchical data sets are combined via
[`mergeLabels`](https://beckerbenj.github.io/eatGADS/reference/mergeLabels.md)
and the data base is created via
[`createGADS`](https://beckerbenj.github.io/eatGADS/reference/createGADS.md).
For this, the package `eatDB` is utilized. See also
[`createDB`](https://rdrr.io/pkg/eatDB/man/createDB.html).

## Using the GADS

The content of a data base can be obtained via
[`namesGADS`](https://beckerbenj.github.io/eatGADS/reference/namesGADS.md).
Data is extracted from the data base via
[`getGADS`](https://beckerbenj.github.io/eatGADS/reference/getGADS.md)
for a single GADS and via
[`getTrendGADS`](https://beckerbenj.github.io/eatGADS/reference/getTrendGADS.md)
for trend analysis. The resulting object is a `GADSdat` object. Meta
data can be extracted via
[`extractMeta`](https://beckerbenj.github.io/eatGADS/reference/extractMeta.md),
either from the `GADSdat` object or directly from the data base. Data
can be extracted from the `GADSdat` object via
[`extractData`](https://beckerbenj.github.io/eatGADS/reference/extractData.md).

## See also

Useful links:

- <https://github.com/beckerbenj/eatGADS>

- <https://beckerbenj.github.io/eatGADS/>

## Author

**Maintainer**: Benjamin Becker <b.becker@iqb.hu-berlin.de>

Other contributors:

- Karoline Sachse \[contributor\]

- Johanna Busse \[contributor\]
