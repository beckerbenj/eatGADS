# Changelog

## eatGADS 1.2.0.9000

### new features

- [`checkIntOverflow()`](https://beckerbenj.github.io/eatGADS/reference/checkIntOverflow.md)
  and
  [`checkLabeledFractionals()`](https://beckerbenj.github.io/eatGADS/reference/checkLabeledFractionals.md)
  check if a `GADSdat` complies with Stata’s requirements for data sets
  ([\#110](https://github.com/beckerbenj/eatGADS/issues/110))
- [`checkVarNames()`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)
  can now also check the lengths of variable names against
  program-specific restrictions
  ([\#110](https://github.com/beckerbenj/eatGADS/issues/110))
- A list of limits to dataset components under Stata and SPSS is
  available in `program_limits` and via its access function
  [`getProgramLimit()`](https://beckerbenj.github.io/eatGADS/reference/getProgramLimit.md).
- [`check4Stata()`](https://beckerbenj.github.io/eatGADS/reference/check4Stata.md)
  checks if a `GADSdat` complies with all relevant limits Stata imposes
  on datasets
  ([\#110](https://github.com/beckerbenj/eatGADS/issues/110)).

### bug fixes

- [`autoRecode()`](https://beckerbenj.github.io/eatGADS/reference/autoRecode.md)
  no longer fails while applying a template that does not yet cover a
  value which occurs in the data more than once
  ([\#131](https://github.com/beckerbenj/eatGADS/issues/131))
- [`autoRecode()`](https://beckerbenj.github.io/eatGADS/reference/autoRecode.md)
  correctly preserves NA while applying a template
  ([\#146](https://github.com/beckerbenj/eatGADS/issues/146))
- [`changeMissings()`](https://beckerbenj.github.io/eatGADS/reference/changeMissings.md)
  no longer throws an existing value error in edge cases with multiple
  existing value labels
- [`recodeGADS()`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
  now correctly performs potentially sequential recodes for unlabeled
  values ([\#134](https://github.com/beckerbenj/eatGADS/issues/134))
- arguments in
  [`checkVarNames()`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)
  now turn on/off different checks correctly
  ([\#138](https://github.com/beckerbenj/eatGADS/issues/138))

### documentation

- The behavior of
  [`autoRecode()`](https://beckerbenj.github.io/eatGADS/reference/autoRecode.md)
  is further clarified, emphasizing the dropping of value labels and
  pointing to
  [`multiChar2fac()`](https://beckerbenj.github.io/eatGADS/reference/multiChar2fac.md)
  ([\#136](https://github.com/beckerbenj/eatGADS/issues/136)).

## eatGADS 1.2.0

CRAN release: 2025-05-21

### new features

- [`dropDuplicateIDs()`](https://beckerbenj.github.io/eatGADS/reference/dropDuplicateIDs.md)
  allows dropping duplicate IDs based on number of missings on selected
  variables ([\#67](https://github.com/beckerbenj/eatGADS/issues/67))
- [`changeValLabels()`](https://beckerbenj.github.io/eatGADS/reference/changeValLabels.md)
  and
  [`changeMissings()`](https://beckerbenj.github.io/eatGADS/reference/changeMissings.md)
  allow changing value labels and missing tags for multiple variables at
  once ([\#33](https://github.com/beckerbenj/eatGADS/issues/33))
- [`recodeGADS()`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
  allows recoding multiple variables at once
  ([\#107](https://github.com/beckerbenj/eatGADS/issues/107))
- [`merge.GADSdat()`](https://beckerbenj.github.io/eatGADS/reference/merge.GADSdat.md)
  allows automatically assigning NAs created through merging a missing
  code and a value label
  ([\#1](https://github.com/beckerbenj/eatGADS/issues/1))
- [`equalData()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
  and
  [`equalMeta()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
  now split the functionality of
  [`equalGADS()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
- [`equalData()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
  and
  [`equalGADS()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
  now support multiple id variables (`id`)

### changes

- [`inspectMetaDifferences()`](https://beckerbenj.github.io/eatGADS/reference/inspectMetaDifferences.md)
  now provides consistent output for variable label differences

### bug fixes

- [`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
  and
  [`extractData()`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)
  no longer throw an error if multiple values of the same variable are
  labeled `NA` ([\#96](https://github.com/beckerbenj/eatGADS/issues/96))
- [`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
  and
  [`extractData()`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)
  no longer produce an error if there are multiple duplicate value
  labels in a variable
- [`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
  and
  [`extractData()`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)
  no longer produce a warning if multiple duplicate value labels occur
  which are tagged and transformed to `NA` anyway
  ([\#98](https://github.com/beckerbenj/eatGADS/issues/98))
- [`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
  and
  [`extractData()`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)
  now provide consistent output for values which have `NA` as value
  label ([\#100](https://github.com/beckerbenj/eatGADS/issues/100))
- [`inspectMetaDifferences()`](https://beckerbenj.github.io/eatGADS/reference/inspectMetaDifferences.md)
  now correctly reports differences for meta data with differing row
  names ([\#102](https://github.com/beckerbenj/eatGADS/issues/102))
- hot fix for `existingMeta` argument in
  [`recodeGADS()`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
  and
  [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md),
  which was ignored when values were recoded into each other
  ([\#104](https://github.com/beckerbenj/eatGADS/issues/104))
- fix unclear documentation in
  [`removeValLabels()`](https://beckerbenj.github.io/eatGADS/reference/removeValLabels.md)
  ([\#111](https://github.com/beckerbenj/eatGADS/issues/111))
- fix
  [`changeMissings()`](https://beckerbenj.github.io/eatGADS/reference/changeMissings.md)
  bug which dropped a single, existing value label and threw an error
  ([\#121](https://github.com/beckerbenj/eatGADS/issues/121))
- [`checkMissingValLabels()`](https://beckerbenj.github.io/eatGADS/reference/checkEmptyValLabels.md)
  now correctly recognizes large integers
  ([\#124](https://github.com/beckerbenj/eatGADS/issues/124))
- Checks for case insensitive duplicate variable names are now run by
  [`checkVarNames()`](https://beckerbenj.github.io/eatGADS/reference/checkVarNames.md)
  and not outside of it
- [`equalData()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
  and
  [`equalGADS()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
  now work with `GADSdat` objects of length one
  ([\#127](https://github.com/beckerbenj/eatGADS/issues/127))

## eatGADS 1.1.1

CRAN release: 2024-10-09

### new features

- [`inspectDifferences()`](https://beckerbenj.github.io/eatGADS/reference/inspectDifferences.md)
  and
  [`inspectMetaDifferences()`](https://beckerbenj.github.io/eatGADS/reference/inspectMetaDifferences.md)
  now allow comparisons of variables within the same `GADSdat` object
  ([\#62](https://github.com/beckerbenj/eatGADS/issues/62))
- new function
  [`import_tibble()`](https://beckerbenj.github.io/eatGADS/reference/import_tibble.md)
  allows importing `tibbles` as `GADSdat` objects
  ([\#88](https://github.com/beckerbenj/eatGADS/issues/88))

### bug fixes

- [`applyLookup()`](https://beckerbenj.github.io/eatGADS/reference/applyLookup.md)
  now works for multiple variables that contain `NAs`
  ([\#68](https://github.com/beckerbenj/eatGADS/issues/68))
- [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  and
  [`recodeGADS()`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
  now correctly perform recodings (and throw errors) if multiple meta
  data conflicts occur
  ([\#57](https://github.com/beckerbenj/eatGADS/issues/57))
- [`removeEmptyValLabels()`](https://beckerbenj.github.io/eatGADS/reference/removeEmptyValLabels.md)
  for removing unused missing tags and value labels
  ([\#4](https://github.com/beckerbenj/eatGADS/issues/4))
- [`extractData()`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)
  and
  [`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
  now correctly apply value labels, even when value and value label
  conflicts exist
- [`write_spss2()`](https://beckerbenj.github.io/eatGADS/reference/write_spss2.md)
  now also handles variables containing only NA values when `format` is
  also NA ([\#72](https://github.com/beckerbenj/eatGADS/issues/72))
- [`changeMissings()`](https://beckerbenj.github.io/eatGADS/reference/changeMissings.md)
  now correctly changes missing labels for values of variables with
  partially non-existent `value`s and/or `valLabel`s
  ([\#73](https://github.com/beckerbenj/eatGADS/issues/73))
- [`autoRecode()`](https://beckerbenj.github.io/eatGADS/reference/autoRecode.md)
  now correctly overwrites the existing variable if `var_suffix = ""`
  ([\#84](https://github.com/beckerbenj/eatGADS/issues/84))
- The output of
  [`inspectMetaDifferences()`](https://beckerbenj.github.io/eatGADS/reference/inspectMetaDifferences.md)
  is now correctly named even if differences in variable labels and
  `SPSS` format occur
  ([\#81](https://github.com/beckerbenj/eatGADS/issues/81))
- [`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
  and
  [`extractData()`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)
  now correctly transform variables with duplicate value labels
  ([\#77](https://github.com/beckerbenj/eatGADS/issues/77))

### documentation

- `import_spss` vignette updated to use
  [`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
  instead of
  [`extractData()`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)

### internal

- refactored
  [`extractData()`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)
  to use
  [`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
  internally ([\#82](https://github.com/beckerbenj/eatGADS/issues/82))

## eatGADS 1.1.0

CRAN release: 2023-08-25

### new features

- [`equalGADS()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
  now allows pre-sorting the data by an identifier variable
- [`updateMeta()`](https://beckerbenj.github.io/eatGADS/reference/updateMeta.md),
  [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md),
  [`changeVarNames()`](https://beckerbenj.github.io/eatGADS/reference/changeVarNames.md),
  [`cloneVariable()`](https://beckerbenj.github.io/eatGADS/reference/cloneVariable.md),
  [`createVariable()`](https://beckerbenj.github.io/eatGADS/reference/createVariable.md),
  [`composeVar()`](https://beckerbenj.github.io/eatGADS/reference/composeVar.md),
  and
  [`dummies2char()`](https://beckerbenj.github.io/eatGADS/reference/dummies2char.md)
  now have optional checks of new variables names via the
  `checkVarNames` argument
- [`reuseMeta()`](https://beckerbenj.github.io/eatGADS/reference/reuseMeta.md)
  now can be use on multiple variables at once
- [`inspectMetaDifferences()`](https://beckerbenj.github.io/eatGADS/reference/inspectMetaDifferences.md)
  now can be applied to data bases as well
- [`recodeNA2missing()`](https://beckerbenj.github.io/eatGADS/reference/recodeNA2missing.md)
  for recoding `NAs` to a specific missing code
- [`recode2NA()`](https://beckerbenj.github.io/eatGADS/reference/recode2NA.md)
  now allows the recoding of multiple `values` at once and returns a
  warning, if the recoded values have existing value labels in the
  recoded variables
- [`updateMeta()`](https://beckerbenj.github.io/eatGADS/reference/updateMeta.md)
  is now compatible with
  [`extractData()`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)
  and
  [`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
- S3 method of
  [`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
  now available for `trend_GADSdat` objects
- [`recodeGADS()`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
  and
  [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  allow recoding values without recoding value labels (via
  `existingMeta = "ignore"`)
- [`insertVariable()`](https://beckerbenj.github.io/eatGADS/reference/insertVariable.md)
  has been renamed
  [`relocateVariable()`](https://beckerbenj.github.io/eatGADS/reference/relocateVariable.md)
  for clarity. Variables can now be inserted at the very beginning of a
  `GADSdat`
- [`cloneVariable()`](https://beckerbenj.github.io/eatGADS/reference/cloneVariable.md)
  and
  [`autoRecode()`](https://beckerbenj.github.io/eatGADS/reference/autoRecode.md)
  now allow automatic appending of variable label suffixes via the
  `label_suffix` argument
- new function
  [`emptyTheseVariables()`](https://beckerbenj.github.io/eatGADS/reference/emptyTheseVariables.md)
  allows setting multiple variables to `NA`

### bug fixes

- [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  now removes duplicate meta data rows, which caused an error in, e.g.,
  [`recodeGADS()`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
- [`export_tibble()`](https://beckerbenj.github.io/eatGADS/reference/export_tibble.md)
  and
  [`write_spss()`](https://beckerbenj.github.io/eatGADS/reference/write_spss.md)
  now throw an error if a conversion of four or more discrete missing
  tags into a missing range has undesired side effects
- bug fix in
  [`checkMissingsByValues()`](https://beckerbenj.github.io/eatGADS/reference/checkMissings.md),
  now correctly reports missing tags outside of the specified value
  range
- bug fix in
  [`cloneVariable()`](https://beckerbenj.github.io/eatGADS/reference/cloneVariable.md),
  now new variables names which are also `SQLite` keywords no longer
  throw a error (fixed conflicts caused by `checkVarName()`)

## eatGADS 1.0.0

CRAN release: 2023-04-06

### new features

- [`multiChar2fac()`](https://beckerbenj.github.io/eatGADS/reference/multiChar2fac.md)
  now allows converting to upper or lower cases via the `convertCases`
  argument
- [`checkMissingsByValues()`](https://beckerbenj.github.io/eatGADS/reference/checkMissings.md)
  for checking missing tags for specific, labeled values (or a value
  range)
- [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  with more informative error message
- [`checkMissingValLabels()`](https://beckerbenj.github.io/eatGADS/reference/checkEmptyValLabels.md)
  now allows checks for specific variable classes
- [`checkMissingValLabels()`](https://beckerbenj.github.io/eatGADS/reference/checkEmptyValLabels.md)
  now offers `data.frame` output format
- [`extractData2()`](https://beckerbenj.github.io/eatGADS/reference/extractData2.md)
  for better usability and support of ordered factors
- [`fixEncoding()`](https://beckerbenj.github.io/eatGADS/reference/fixEncoding.md)
  now supports imports of `windows-1250` imported files via
  [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  with `UTF-8` encoding
- [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  now transforms `DATEATIME` and `ADATE` variables to character,
  assigning an appropriate format
- [`cloneVariable()`](https://beckerbenj.github.io/eatGADS/reference/cloneVariable.md)
  for duplicating an existing variable under a new name
- [`createVariable()`](https://beckerbenj.github.io/eatGADS/reference/createVariable.md)
  for creating a new, empty variable
- [`insertVariable()`](https://beckerbenj.github.io/eatGADS/reference/insertVariable.md)
  for changing the position of a variable in the data set
- [`autoRecode()`](https://beckerbenj.github.io/eatGADS/reference/autoRecode.md)
  for automatically recoding (numerical) variables

### bug fixes

- bug fix in
  [`multiChar2fac()`](https://beckerbenj.github.io/eatGADS/reference/multiChar2fac.md)
  for cases with existing value labels (with values labeled greater than
  zero)
- [`composeVar()`](https://beckerbenj.github.io/eatGADS/reference/composeVar.md)
  now correctly prioritizes if both variables have missing values
- [`changeMissings()`](https://beckerbenj.github.io/eatGADS/reference/changeMissings.md)
  now does no longer add arbitrary value labels for newly tagged values
- bug fix in
  [`checkUniqueness2()`](https://beckerbenj.github.io/eatGADS/reference/checkUniqueness2.md)
  for cases with differing numbers of cases per imputed data set
- bug fix in
  [`changeValLabels()`](https://beckerbenj.github.io/eatGADS/reference/changeValLabels.md)
  for a mixture of already existing and new values
- bug fixes in
  [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  when setting `labeledStirngs = "transform"`
- bug fix in
  [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  for variables with missing tags but no value labels (these missing
  tags were dropped before)

## eatGADS 0.20.0

CRAN release: 2022-06-24

- [`extractData()`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)
  adds now all variable labels as a `label` attribute to the resulting
  `data.frame`
- a numeric tolerance can now be specified within
  [`equalGADS()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
- fixed warnings for
  [`getTrendGADS()`](https://beckerbenj.github.io/eatGADS/reference/getTrendGADS.md)
- [`inspectMetaDifferences()`](https://beckerbenj.github.io/eatGADS/reference/inspectMetaDifferences.md)
  for inspecting meta differences between two `GADSdats` and a single
  variable
- [`checkUniqueness2()`](https://beckerbenj.github.io/eatGADS/reference/checkUniqueness2.md)
  for faster checking if a variable is unique within an identifier
  variable
- [`fixEncoding()`](https://beckerbenj.github.io/eatGADS/reference/fixEncoding.md)
  for fixing encoding issues caused during the import via
  [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  using `ASCII` or other encoding
- [`composeVar()`](https://beckerbenj.github.io/eatGADS/reference/composeVar.md)
  for combining the information of two variables in a third variable
- [`subImputations()`](https://beckerbenj.github.io/eatGADS/reference/subImputations.md)
  now works with differing `varName` and `varName_imp`
- [`fillImputations()`](https://beckerbenj.github.io/eatGADS/reference/fillImputations.md)
  for efficiently filling missing values in imputed variables via not
  imputed variables
- [`checkFormat()`](https://beckerbenj.github.io/eatGADS/reference/checkFormat.md)
  now works correctly for variables without value labels
- [`checkFormat()`](https://beckerbenj.github.io/eatGADS/reference/checkFormat.md)
  now works correctly for variables with purely `NAs`
- [`import_raw()`](https://beckerbenj.github.io/eatGADS/reference/import_raw.md)
  now works correctly if variables are provided as integers
- [`reuseMeta()`](https://beckerbenj.github.io/eatGADS/reference/reuseMeta.md)
  now works correctly if variables occur in multiple sheets in a data
  base or `all_GADSdat`

## eatGADS 0.19.1

CRAN release: 2022-01-27

- fix permanent URL redirect in `README`

## eatGADS 0.19.0

### breaking changes

- [`getTrendGADS()`](https://beckerbenj.github.io/eatGADS/reference/getTrendGADS.md)
  now supports multiple measurement points but support for linking
  errors has been dropped
- [`extractData()`](https://beckerbenj.github.io/eatGADS/reference/extractData.md)
  now supports multiple measurement points but support for linking
  errors has been dropped
- trend example data bases implemented
- [`getTrendGADSOld()`](https://beckerbenj.github.io/eatGADS/reference/getTrendGADSOld.md)
  and
  [`extractDataOld()`](https://beckerbenj.github.io/eatGADS/reference/extractDataOld.md)
  provide backward compatibility
- `checkLEStructure()` deprecated
- [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  now can automatically transform labeled or missing tagged character
  values via the changed `labeledStrings` argument

### major changes

- [`assimilateValLabels()`](https://beckerbenj.github.io/eatGADS/reference/assimilateValLabels.md)
  for assimilating value labels of multiple variables
- [`cbind.GADSdat()`](https://beckerbenj.github.io/eatGADS/reference/cbind.GADSdat.md)
  method for binding multiple `GADSdat` objects by column
- [`calculateScale()`](https://beckerbenj.github.io/eatGADS/reference/calculateScale.md)
  for calculating scales from item sets
- [`checkUniqueness()`](https://beckerbenj.github.io/eatGADS/reference/checkUniqueness.md)
  for checking uniqueness of a variable within an `id` variable

### minor changes

- clarifications and additional explanations in vignettes (`meta_data`)
- bug fix
  [`updateMeta()`](https://beckerbenj.github.io/eatGADS/reference/updateMeta.md)
  (all added variables are now checked for illegal naming)

## eatGADS 0.18.1

CRAN release: 2021-10-06

- [`equalGADS()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
  now ignores irrelevant format differences (`F8.0` vs `F8`)
- fix `JSS` link in vignette

## eatGADS 0.18.0

- [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  and
  [`changeValLabels()`](https://beckerbenj.github.io/eatGADS/reference/changeValLabels.md)
  now automatically assign `"valid"` to the `missings` columns for
  labeled values
- bug fix
  [`equalGADS()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
  (meta data differences solely due to different meta data sorting are
  now ignored)
- [`equalGADS()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
  can now exclude certain meta data columns from the comparison
- [`checkFormat()`](https://beckerbenj.github.io/eatGADS/reference/checkFormat.md)
  for checking and modifying`SPSS` format type compatibility with actual
  data
- [`check4SPSS()`](https://beckerbenj.github.io/eatGADS/reference/check4SPSS.md)
  for checking `SPSS` meta data conventions and requirements
- [`changeVarNames()`](https://beckerbenj.github.io/eatGADS/reference/changeVarNames.md)
  and
  [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  now enforce `SQLite3` naming conventions
- bug fixes
  [`write_spss2()`](https://beckerbenj.github.io/eatGADS/reference/write_spss2.md)
- bug fix
  [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md):
  now recognizes differences in `NAs` in old columns when checking the
  change table

## eatGADS 0.17.0

CRAN release: 2021-07-19

- [`inspectDifferences()`](https://beckerbenj.github.io/eatGADS/reference/inspectDifferences.md)
  now checks whether variable is of the same type in both `GADSdats`
- [`subImputations()`](https://beckerbenj.github.io/eatGADS/reference/subImputations.md)
  for substituting imputations with original, not imputed values
- [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  now returns a more informative error if the variable sets in the
  `GADSdat` and the `changeTable` differ
- [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  tries to convert `value` columns in `changeTable` to numeric before
  throwing an error
- [`dummies2char()`](https://beckerbenj.github.io/eatGADS/reference/dummies2char.md)
  for recoding a set of dummy variables to a set of left filled
  character variables
- [`fac2dummies()`](https://beckerbenj.github.io/eatGADS/reference/fac2dummies.md)
  and
  [`fac2dummies_complex()`](https://beckerbenj.github.io/eatGADS/reference/fac2dummies_complex.md)
  for recoding a labeled factor variable to multiple labeled dummy
  variables
- [`equalGADS()`](https://beckerbenj.github.io/eatGADS/reference/equalGADS.md)
  for comparing the complete structure and content of two `GADSdat`
  objects
- [`inspectDifferences()`](https://beckerbenj.github.io/eatGADS/reference/inspectDifferences.md)
  for comparing specific variables
- [`reuseMeta()`](https://beckerbenj.github.io/eatGADS/reference/reuseMeta.md)
  now allows only transferring value labels for values coded as missing
- [`checkEmptyValLabels()`](https://beckerbenj.github.io/eatGADS/reference/checkEmptyValLabels.md)
  and
  [`checkMissingValLabels()`](https://beckerbenj.github.io/eatGADS/reference/checkEmptyValLabels.md)
  provide cleaner output
- [`write_spss2()`](https://beckerbenj.github.io/eatGADS/reference/write_spss2.md)
  for writing `.sav` files via a text file and a `SPSS` syntax
- [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  and
  [`write_spss()`](https://beckerbenj.github.io/eatGADS/reference/write_spss.md)
  now deal correctly with missing codes for character variables
- [`recodeGADS()`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
  allows recoding of unlabeled variables and values
- [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  now takes an `encoding` argument to work around faulty defaults

### Internal

- better performance of
  [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  on value level
- bug fix
  [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  (all columns are forced to be `double` when imported instead of
  `integer`)
- bug fix
  [`fac2dummies_complex()`](https://beckerbenj.github.io/eatGADS/reference/fac2dummies_complex.md)
  (value labels of dummy variables are tagged as `valid`)
- bug fixes
  [`recodeGADS()`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
- bug fix
  [`removeValLabels()`](https://beckerbenj.github.io/eatGADS/reference/removeValLabels.md)
  (when a single value label was removed)
- bug fixes
  [`collapseMultiMC_Text()`](https://beckerbenj.github.io/eatGADS/reference/collapseMultiMC_Text.md)
  (`left_fill()` and `drop_empty()`)
- bug fix
  [`remove2NAchar()`](https://beckerbenj.github.io/eatGADS/reference/remove2NAchar.md)
  (with partially labeled variables)
- bug fix
  [`multiChar2fac()`](https://beckerbenj.github.io/eatGADS/reference/multiChar2fac.md)
  (with partially labeled variables)
- bug fix
  [`applyLookup_expandVar()`](https://beckerbenj.github.io/eatGADS/reference/applyLookup_expandVar.md)
  (removed unnecessary messages)
- warnings removed for
  [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  (`haven` patch for labeled character variables)

## eatGADS 0.16.0

CRAN release: 2021-02-23

- [`checkEmptyValLabels()`](https://beckerbenj.github.io/eatGADS/reference/checkEmptyValLabels.md)
  and
  [`checkMissingValLabels()`](https://beckerbenj.github.io/eatGADS/reference/checkEmptyValLabels.md)
  for comparing values in the data and value labels
- [`compareGADS()`](https://beckerbenj.github.io/eatGADS/reference/compareGADS.md)
  for comparing multiple variables between two `GADSdats`
- [`recodeGADS()`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
  allows recoding (multiple) values into existing values now, argument
  `existingMeta` added
- [`recodeGADS()`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
  allows recoding of `NA` now
- [`recode2NA()`](https://beckerbenj.github.io/eatGADS/reference/recode2NA.md)
  allows recoding of numeric and character values to `NA`
- [`recodeString2NA()`](https://beckerbenj.github.io/eatGADS/reference/recodeString2NA.md)
  has been deprecated
- bug fix
  [`collapseColumns()`](https://beckerbenj.github.io/eatGADS/reference/collapseColumns.md)
  (now supports `new_value` as column name in look up table)
- [`checkValue()`](https://beckerbenj.github.io/eatGADS/reference/checkValue.md)
  now returns occurrence count as vector instead of giving a message
- [`checkValue()`](https://beckerbenj.github.io/eatGADS/reference/checkValue.md)
  now works on a subset of variables (via argument `vars`)
- [`checkValue()`](https://beckerbenj.github.io/eatGADS/reference/checkValue.md)
  now supports checking for `NA`
- [`removeValLabels()`](https://beckerbenj.github.io/eatGADS/reference/removeValLabels.md)
  now can remove only specific `value` - `valLabel` pairs if required
- changing value labels inside of
  [`recodeGADS()`](https://beckerbenj.github.io/eatGADS/reference/recodeGADS.md)
  via `newValueLabels` deprecated (use
  [`changeValLabels()`](https://beckerbenj.github.io/eatGADS/reference/changeValLabels.md)instead)
- [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  now works with `data.frames` (for compatibility with `excel` import
  and export)
- [`import_spss()`](https://beckerbenj.github.io/eatGADS/reference/import_spss.md)
  now supports variables of type `datetime`
- [`import_stata()`](https://beckerbenj.github.io/eatGADS/reference/import_stata.md)
  and
  [`write_stata()`](https://beckerbenj.github.io/eatGADS/reference/write_spss.md)
  for exporting and importing `Stata` files

### Internal

- `check_GADSdat()` now checks for duplicate value rows
- switch from `travis` and `appVeyor` to `Github Action`
- [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  checks `format_new` column now for illegal entries
- [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  throws error if result has corrupted meta data
- [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  orders meta data within a variable (by ascending value)
- better performance of
  [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  on value level
- bug fix for
  [`applyChangeMeta()`](https://beckerbenj.github.io/eatGADS/reference/applyChangeMeta.md)
  if only `missings_new` is changed
- Setup `revdepcheck`

## eatGADS 0.15.2

CRAN release: 2020-11-25

- Initial release on CRAN.
