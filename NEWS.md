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
* `import_stata()` and `write_stata()` for exporting and importing Stata files

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
