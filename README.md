# eatGADS

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/eatGADS)](https://CRAN.R-project.org/package=eatGADS)
[![R-CMD-check](https://github.com/beckerbenj/eatGADS/workflows/R-CMD-check/badge.svg)](https://github.com/beckerbenj/eatGADS/actions)
[![Codecov test coverage](https://codecov.io/gh/beckerbenj/eatGADS/branch/master/graph/badge.svg)](https://app.codecov.io/gh/beckerbenj/eatGADS?branch=master)
[![](http://cranlogs.r-pkg.org/badges/grand-total/eatGADS?color=blue)](https://cran.r-project.org/package=eatGADS)

<!-- badges: end -->

## Overview

`eatGADS` (educational assessment tools: GADS) is the data management and data handling tool used by the Institute for Educational Quality Improvement in Germany (`IQB`). It has a strong focus of handling and processing meta data.

`eatGADS` uses [SQLite3](https://www.sqlite.org/index.html) as a back end and is especially suited for importing data stored as `SPSS` files. [eatDB](https://github.com/beckerbenj/eatDB) is used for data base creating and use, [haven](https://github.com/tidyverse/haven) is used for importing SPSS files.

## Installation

```R
# Install stable version from CRAN via
install.packages("eatGADS")

# Install development version from GitHub via
remotes::install_github("beckerbenj/eatGADS", build_vignettes = TRUE, dependencies = TRUE)
```

## Vignettes

The functionality of `eatGADS` is extensively documented in various vignettes. If you have questions regarding existing functionality or requests for new features, contact the package author.

```R
## See vignettes for exhaustive documentation
# overview over all vignettes
vignette(package = "eatGADS")

# see a specific vignettes
vignette("import_spss") # how to import spss data?
vignette("getGADS") # how to use a eatGADS data base?
```
