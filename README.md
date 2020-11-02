# eatGADS

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/beckerbenj/eatGADS.svg?branch=master)](https://travis-ci.org/beckerbenj/eatGADS)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/beckerbenj/eatGADS?branch=master&svg=true)](https://ci.appveyor.com/project/beckerbenj/eatGADS)
[![Codecov test coverage](https://codecov.io/gh/beckerbenj/eatGADS/branch/master/graph/badge.svg)](https://codecov.io/gh/beckerbenj/eatGADS?branch=master)
<!-- badges: end -->

## Overview

`eatGADS` (educational assessment tools: GADS) is the data management and data handling tool used by the Institute for Educational Quality Improvement  in Germany (`IQB`). It uses `SQLlite3` as a back end and is especially suited for importing data stored as `SPSS` files. `eatDB` is used for data base creating and use, `haven` is used for importing SPSS files.

## Installation

```R
# Install eatGADS from GitHub via
remotes::install_github("beckerbenj/eatGADS", build_vignettes = TRUE,
                        dependencies = TRUE)
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
