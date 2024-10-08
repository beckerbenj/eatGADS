# eatGADS 1.1.1
## new features
* `inspectDifferences()` and `inspectMetaDifferences()` now allow comparisons of variables within the same `GADSdat` object (#62)
* new function `import_tibble()` allows importing `tibbles` as `GADSdat` objects (#88)

## bug fixes
* `applyLookup()` now works for multiple variables that contain `NAs` (#68)
* `applyChangeMeta()` and `recodeGADS()` now correctly perform recodings (and throw errors) if multiple meta data conflicts occur (#57)
* `removeEmptyValLabels()` for removing unused missing tags and value labels (#4)
* `extractData()` and `extractData2()` now correctly apply value labels, even when value and value label conflicts exist
* `write_spss2()` now also handles variables containing only NA values when `format` is also NA (#72)
* `changeMissings()` now correctly changes missing labels for values of variables with partially non-existent `value`s and/or `valLabel`s (#73)
* `autoRecode()` now correctly overwrites the existing variable if `var_suffix = ""` (#84)
* The output of `inspectMetaDifferences()` is now correctly named even if differences in variable labels and `SPSS` format occur (#81)
* `extractData2()` and `extractData()` now correctly transform variables with duplicate value labels (#77)

## documentation
* `import_spss` vignette updated to use `extractData2()` instead of `extractData()`

## internal
* refactored `extractData()` to use `extractData2()` internally (#82)

# eatGADS 1.1.0

## new features
* `equalGADS()` now allows pre-sorting the data by an identifier variable
* `updateMeta()`, `applyChangeMeta()`, `changeVarNames()`, `cloneVariable()`, `createVariable()`, `composeVar()`, and `dummies2char()` now have optional checks of new variables names via the `checkVarNames` argument
* `reuseMeta()` now can be use on multiple variables at once
* `inspectMetaDifferences()` now can be applied to data bases as well
* `recodeNA2missing()` for recoding `NAs` to a specific missing code
* `recode2NA()` now allows the recoding of multiple `values` at once and returns a warning, if the recoded values have existing value labels in the recoded variables
* `updateMeta()` is now compatible with `extractData()` and `extractData2()`
* S3 method of `extractData2()` now available for `trend_GADSdat` objects
* `recodeGADS()` and `applyChangeMeta()` allow recoding values without recoding value labels (via `existingMeta = "ignore"`)
* `insertVariable()` has been renamed `relocateVariable()` for clarity. Variables can now be inserted at the very beginning of a `GADSdat`
* `cloneVariable()` and `autoRecode()` now allow automatic appending of variable label suffixes via the `label_suffix` argument
* new function `emptyTheseVariables()` allows setting multiple variables to `NA`

## bug fixes
* `import_spss()` now removes duplicate meta data rows, which caused an error in, e.g., `recodeGADS()`
* `export_tibble()` and `write_spss()` now throw an error if a conversion of four or more discrete missing tags into a missing range has undesired side effects
* bug fix in `checkMissingsByValues()`, now correctly reports missing tags outside of the specified value range
* bug fix in `cloneVariable()`, now new variables names which are also `SQLite` keywords no longer throw a error (fixed conflicts caused by `checkVarName()`)


# eatGADS 1.0.0

## new features
* `multiChar2fac()` now allows converting to upper or lower cases via the `convertCases` argument
* `checkMissingsByValues()` for checking missing tags for specific, labeled values (or a value range)
* `import_spss()` with more informative error message
* `checkMissingValLabels()` now allows checks for specific variable classes
* `checkMissingValLabels()` now offers `data.frame` output format
* `extractData2()` for better usability and support of ordered factors
* `fixEncoding()` now supports imports of `windows-1250` imported files via `import_spss()` with `UTF-8` encoding
* `import_spss()` now transforms `DATEATIME` and `ADATE` variables to character, assigning an appropriate format
* `cloneVariable()` for duplicating an existing variable under a new name
* `createVariable()` for creating a new, empty variable
* `insertVariable()` for changing the position of a variable in the data set
* `autoRecode()` for automatically recoding (numerical) variables

## bug fixes
* bug fix in `multiChar2fac()` for cases with existing value labels (with values labeled greater than zero)
* `composeVar()` now correctly prioritizes if both variables have missing values
* `changeMissings()` now does no longer add arbitrary value labels for newly tagged values
* bug fix in `checkUniqueness2()` for cases with differing numbers of cases per imputed data set
* bug fix in `changeValLabels()` for a mixture of already existing and new values
* bug fixes in `import_spss()` when setting `labeledStirngs = "transform"`
* bug fix in `import_spss()` for variables with missing tags but no value labels (these missing tags were dropped before)


# eatGADS 0.20.0

* `extractData()` adds now all variable labels as a `label` attribute to the resulting `data.frame`
* a numeric tolerance can now be specified within `equalGADS()`
* fixed warnings for `getTrendGADS()`
* `inspectMetaDifferences()` for inspecting meta differences between two `GADSdats` and a single variable
* `checkUniqueness2()` for faster checking if a variable is unique within an identifier variable
* `fixEncoding()` for fixing encoding issues caused during the import via `import_spss()` using `ASCII` or other encoding
* `composeVar()` for combining the information of two variables in a third variable
* `subImputations()` now works with differing `varName` and `varName_imp`
* `fillImputations()` for efficiently filling missing values in imputed variables via not imputed variables
* `checkFormat()` now works correctly for variables without value labels
* `checkFormat()` now works correctly for variables with purely `NAs`
* `import_raw()` now works correctly if variables are provided as integers
* `reuseMeta()` now works correctly if variables occur in multiple sheets in a data base or `all_GADSdat`


# eatGADS 0.19.1

* fix permanent URL redirect in `README`


# eatGADS 0.19.0

## breaking changes
* `getTrendGADS()` now supports multiple measurement points but support for linking errors has been dropped
* `extractData()` now supports multiple measurement points but support for linking errors has been dropped
* trend example data bases implemented
* `getTrendGADSOld()` and `extractDataOld()` provide backward compatibility
* `checkLEStructure()` deprecated
* `import_spss()` now can automatically transform labeled or missing tagged character values via the changed `labeledStrings` argument

## major changes
* `assimilateValLabels()` for assimilating value labels of multiple variables
* `cbind.GADSdat()` method for binding multiple `GADSdat` objects by column
* `calculateScale()` for calculating scales from item sets
* `checkUniqueness()` for checking uniqueness of a variable within an `id` variable

## minor changes
* clarifications and additional explanations in vignettes (`meta_data`)
* bug fix `updateMeta()` (all added variables are now checked for illegal naming)


# eatGADS 0.18.1

* `equalGADS()` now ignores irrelevant format differences (`F8.0` vs `F8`)
* fix `JSS` link in vignette


# eatGADS 0.18.0

* `applyChangeMeta()` and `changeValLabels()` now automatically assign `"valid"` to the `missings` columns for labeled values
* bug fix `equalGADS()` (meta data differences solely due to different meta data sorting are now ignored) 
* `equalGADS()` can now exclude certain meta data columns from the comparison
* `checkFormat()` for checking and modifying`SPSS` format type compatibility with actual data
* `check4SPSS()` for checking `SPSS` meta data conventions and requirements
* `changeVarNames()` and `applyChangeMeta()` now enforce `SQLite3` naming conventions
* bug fixes `write_spss2()`
* bug fix `applyChangeMeta()`: now recognizes differences in `NAs` in old columns when checking the change table


# eatGADS 0.17.0

* `inspectDifferences()` now checks whether variable is of the same type in both `GADSdats`
* `subImputations()` for substituting imputations with original, not imputed values
* `applyChangeMeta()` now returns a more informative error if the variable sets in the `GADSdat` and the `changeTable` differ
* `applyChangeMeta()` tries to convert `value` columns in `changeTable` to numeric before throwing an error
* `dummies2char()` for recoding a set of dummy variables to a set of left filled character variables
* `fac2dummies()` and `fac2dummies_complex()` for recoding a labeled factor variable to multiple labeled dummy variables
* `equalGADS()` for comparing the complete structure and content of two `GADSdat` objects
* `inspectDifferences()` for comparing specific variables
* `reuseMeta()` now allows only transferring value labels for values coded as missing
* `checkEmptyValLabels()` and `checkMissingValLabels()` provide cleaner output
* `write_spss2()` for writing `.sav` files via a text file and a `SPSS` syntax
* `import_spss()` and `write_spss()` now deal correctly with missing codes for character variables
* `recodeGADS()` allows recoding of unlabeled variables and values
* `import_spss()` now takes an `encoding` argument to work around faulty defaults


## Internal
* better performance of `applyChangeMeta()` on value level
* bug fix `import_spss()` (all columns are forced to be `double` when imported instead of `integer`)
* bug fix `fac2dummies_complex()` (value labels of dummy variables are tagged as `valid`)
* bug fixes `recodeGADS()`
* bug fix `removeValLabels()` (when a single value label was removed)
* bug fixes `collapseMultiMC_Text()` (`left_fill()` and `drop_empty()`)
* bug fix `remove2NAchar()` (with partially labeled variables)
* bug fix `multiChar2fac()` (with partially labeled variables)
* bug fix `applyLookup_expandVar()` (removed unnecessary messages)
* warnings removed for `import_spss()` (`haven` patch for labeled character variables)


# eatGADS 0.16.0

* `checkEmptyValLabels()` and `checkMissingValLabels()` for comparing values in the data and value labels
* `compareGADS()` for comparing multiple variables between two `GADSdats`
* `recodeGADS()` allows recoding (multiple) values into existing values now, argument `existingMeta` added
* `recodeGADS()` allows recoding of `NA` now
* `recode2NA()` allows recoding of numeric and character values to `NA`
* `recodeString2NA()` has been deprecated
* bug fix `collapseColumns()` (now supports `new_value` as column name in look up table)
* `checkValue()` now returns occurrence count as vector instead of giving a message
* `checkValue()` now works on a subset of variables (via argument `vars`)
* `checkValue()` now supports checking for `NA`
* `removeValLabels()` now can remove only specific `value` - `valLabel` pairs if required 
* changing value labels inside of `recodeGADS()` via `newValueLabels` deprecated (use `changeValLabels()`instead)
* `applyChangeMeta()` now works with `data.frames` (for compatibility with `excel` import and export)
* `import_spss()` now supports variables of type `datetime` 
* `import_stata()` and `write_stata()` for exporting and importing `Stata` files

## Internal
* `check_GADSdat()` now checks for duplicate value rows
* switch from `travis` and `appVeyor` to `Github Action`
* `applyChangeMeta()` checks `format_new` column now for illegal entries
* `applyChangeMeta()` throws error if result has corrupted meta data
* `applyChangeMeta()` orders meta data within a variable (by ascending value)
* better performance of `applyChangeMeta()` on value level
* bug fix for `applyChangeMeta()` if only `missings_new` is changed
* Setup `revdepcheck`


# eatGADS 0.15.2

* Initial release on CRAN.
