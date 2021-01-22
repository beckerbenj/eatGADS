# eatGADS 0.15.2.9000

* `compareGADS()` for comparing multiple variables between two `GADSdats`
* `recodeGADS()` allows recoding of of `NA` now
* `recode2NA()` allows recoding of numeric and character values to `NA`
* `recodeString2NA()` has been deprecated
* bug fix `collapseColumns()` (now supports `new_value` as column name in look up table)
* `checkValue()` now returns occurrence count as vector instead of giving a message
* `removeValLabels()` now can remove only specific `value` - `valLabel` pairs if required 
* changing value labels inside of `recodeGADS()` via `newValueLabels` deprecated (use `changeValLabels()`instead)
* `applyChangeMeta()` now works with `data.frames` (for compatability with `excel` import and export)

## Internal
* `check_GADSdat()` now checks for duplicate value rows


# eatGADS 0.15.2

* Initial release on CRAN.
