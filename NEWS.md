
# eatGADS 0.16.0.9000

* `inspectDifferences()` now checks whether variable is of the same type in both `GADSdats`
* `subImputations()` for substituting imputations with original, not imputed values
* `applyChangeMeta()` now returns a more informative error if the variable sets in the `GADSdat` and the `changeTable` differ
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


# Internal
* better performance of `applyChangeMeta()` on value level
* bug fix `write_save()` (all columns are forced to be `double` when imported instead of `integer`)
* bug fix `fac2dummies_complex()` (value labels of dummy variables are tagged as `valid`)
* bug fixes `recodeGADS()`
* bug fix `removeValLabels()` (when a single value label was removed)
* bug fixes `collapseMultiMC_Text()` (`left_fill()` and `drop_empty()`)
* bug fix `remove2NAchar()` (with partially labeled variables)
* bug fix `multiChar2fac()` (with partially labeled variables)
* bug fix `applyLookup_expandVar()` (removed unnecessary messages)


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
