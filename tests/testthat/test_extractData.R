
# load data with missings
# testM <- import_spss(file = "tests/testthat/helper_spss_missings.sav")
# load(file = "tests/testthat/helper_data.rda")
# testM <- import_spss("tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")
load(file = "helper_data.rda")

control_caching <- FALSE


######## extractData
testM2 <- testM
testM2$dat[, "Var_char"] <- c("a", "b", "c", "d")
testM2$dat[, "Var_char2"] <- c(1, 1, 1, 1)
testM2$labels[8, ] <- c("Var_char", NA, NA, NA, NA, NA, NA, NA)
testM2$labels[9, ] <- c("Var_char2", NA, NA, NA, "labeled", 1, "b_value", NA)
testM2$labels$value <- as.numeric(testM2$labels$value)

test_that("Warnings and errors for Extract Data",  {
  w <- capture_warnings(extractData(testM))
  expect_equal(w[[1]], "Variable VAR1 is partially labeled. Value labels will be dropped for this variable.\nLabeled values are: 1")
  expect_equal(w[[2]], "Variable VAR2 is partially labeled. Value labels will be dropped for this variable.\nLabeled values are: -96")
  expect_error(extractData(testM, convertLabels = "integer"), "Argument convertLabels incorrectly specified.")
})

test_that("Extract data", {
  out <- suppressWarnings(extractData(testM))
  comp <- c(1, NA, NA, 2)
  attr(comp, "label") <- "Variable 1"
  expect_equal(out[, 1], comp)

  out2 <- suppressWarnings(extractData(testM, convertMiss = FALSE))
  comp2 <- c(1, -99, -96, 2)
  attr(comp2, "label") <- "Variable 1"
  expect_equal(out2[, 1], comp2)
  expect_equal(typeof(out$VAR3), "double") ## tests if only missing codes are given, variable is nonetheless transformed to character
})

test_that("Extract data for strings", {
  out <- suppressWarnings(extractData(testM2))
  expect_equal(class(out$Var_char), "character")
  expect_equal(out$Var_char, c("a", "b", "c", "d"))
})

test_that("Extract data for strings into factors", {
  out <- suppressWarnings(extractData(testM2, convertLabels = "factor"))
  expect_equal(class(out$Var_char), "character")
  expect_equal(class(out$Var_char2), "factor")
  expect_equal(out$Var_char2, as.factor(c("b_value", "b_value", "b_value", "b_value")))
})

test_that("char2fac", {
  df <- data.frame(v1 = factor(c("z", "a", "b"), levels = c("z", "a", "b")),
                   stringsAsFactors = TRUE)
  gads <- import_DF(df)
  dat <- extractData(gads, convertLabels = "character")

  out <- char2fac(dat, labels = gads$labels, vars = "v1", convertMiss = TRUE)
  expect_true(is.factor(out$v1))
  expect_false(is.ordered(out$v1))
  expect_equal(as.numeric(out$v1), c(1:3))
})

test_that("varlabels_as_labels", {
  df <- varLabels_as_labels(testM$dat, labels = testM$labels)

  expect_equal(attr(df$VAR1, "label"), "Variable 1")
  expect_equal(attr(df$VAR3, "label"), "Variable 3")
})


 # tests could be rewritten for char2fac
test_that("Correct ordering of factors", {
  df <- data.frame(v1 = factor(c("z", "a", "b"), levels = c("z", "a", "b")),
                   stringsAsFactors = TRUE)
  gads <- import_DF(df)

  dat <- extractData(gads, convertLabels = "factor")
  expect_equal(as.numeric(dat$v1), 1:3)

  gads$labels[3, "missings"] <- "miss"
  dat <- extractData(gads, convertLabels = "factor")
  expect_equal(as.numeric(dat$v1), c(1:2, NA))

  dat2 <- extractData(gads, convertLabels = "factor", convertMiss = FALSE)
  expect_equal(as.numeric(dat2$v1), c(1:3))
})

# tests could be rewritten for char2fac
test_that("Correct behavior if factors can't be sorted", {
  df <- data.frame(v1 = factor(c("z", "a", "b"), levels = c("z", "a", "b")),
                   v2 = factor(c("z", "a", "b"), levels = c("z", "a", "b")),
                   v3 = factor(c("z", "a", "b"), levels = c("z", "a", "b")),
                   stringsAsFactors = TRUE)
  gads <- import_DF(df)
  gads$labels[4, "value"] <- 5
  gads$dat[1, "v2"] <- 5
  gads$labels <- gads$labels[-8, ]

  w <- capture_warnings(dat <- extractData(gads, convertLabels = "factor"))
  expect_equal(w[[2]], "For the following factor variables the underlying integers can not be preserved due to R-incompatible ordering of numeric values: v2")
  expect_equal(as.numeric(dat$v1), c(1, 2, 3))
  expect_equal(as.numeric(dat$v2), c(3, 1, 2))
  expect_equal(as.numeric(dat$v3), c(1:3))

  ## drop partially labeled = FALSE?
  w2 <- capture_warnings(dat2 <- extractData(gads, convertLabels = "factor", dropPartialLabels = FALSE))
  expect_equal(w2[[1]], "For the following factor variables only incomplete value labels are available, rendering the underlying integers meaningless: v3")
  expect_equal(w2[[2]], "For the following factor variables the underlying integers can not be preserved due to R-incompatible ordering of numeric values: v2")
  expect_equal(as.numeric(dat2$v1), c(1, 2, 3))
  expect_equal(as.numeric(dat2$v2), c(3, 1, 2))
  expect_equal(as.character(dat2$v3), c("z", 2, "b"))
  expect_equal(as.numeric(dat2$v3), c(3, 1, 2))

  df3 <- data.frame(v1 = factor(c("z", "a", "b"), levels = c("z", "a", "b")),
                   v3 = factor(c("z", "a", "b"), levels = c("z", "a", "b")),
                   stringsAsFactors = TRUE)
  gads3 <- import_DF(df3)
  gads3$labels <- gads3$labels[-6, ]
  w3 <- capture_warnings(dat3 <- extractData(gads3, convertLabels = "factor", dropPartialLabels = FALSE))
  expect_equal(w3[[1]], "For the following factor variables only incomplete value labels are available, rendering the underlying integers meaningless: v3")
  expect_equal(as.numeric(dat3$v1), c(1, 2, 3))
  expect_equal(as.character(dat3$v3), c("z", "a", 3))
  expect_equal(as.numeric(dat3$v3), c(3, 2, 1))
})

test_that("Correct behavior if not all value labels in actual values", {
  df <- data.frame(v1 = factor(c("z", "a", "b"), levels = c("z", "a", "b")),
                   stringsAsFactors = TRUE)
  gads <- import_DF(df)
  gads$dat[3, "v1"] <- 1

  expect_silent(dat <- extractData(gads, convertLabels = "factor"))
  expect_equal(as.numeric(dat$v1), c(1, 2, 1))
})


mixed_values <- new_GADSdat(dat = data.frame(x = 0, y = 1, stringsAsFactors = FALSE),
                            labels = data.frame(varName = c("x", "y"),
                                varLabel = NA,
                                format = NA,
                                display_width = NA,
                                labeled = c("yes", "yes"),
                                value = c(0, 1),
                                valLabel = c("lab", "lab"),
                                missings = NA, stringsAsFactors = FALSE))

## probably outdated, as strings are no longer supported in value column
test_that("Numerics are kept numeric with extract data", {
  expect_equal(extractData(mixed_values), data.frame(x = "lab", y = "lab", stringsAsFactors = FALSE))
  mixed_values$labels$valLabel <- c(99, 99)
  expect_equal(extractData(mixed_values), data.frame(x = 99, y = 99, stringsAsFactors = FALSE))
})

test_that("ExtractData with DropPartialLabels = TRUE", {
  out <- extractData(testM, dropPartialLabels = FALSE)
  expect_equal(as.character(out$VAR1), c("One", NA, NA, 2))
  expect_equal(as.numeric(out$VAR2), c(1, 1, 1, 1))
})

test_that("ExtractData with some variables labels applied to (convertVariables argument)", {
  # Missing labels (but no variables in the data that show the 'no-conversion'!)
  out <- suppressWarnings(extractData(testM, convertVariables = c("VAR2", "VAR3")))
  expect_equal(as.numeric(out$VAR1), c(1, NA, NA, 2))
  expect_warning(extractData(testM, convertVariables = c()))

  # Two variables with value labels without missings
  label_df <- data.frame(a = c("one", "two"),
                         b = c("three", "four"), stringsAsFactors = TRUE)
  label_df <- import_DF(label_df)
  expect_equal(extractData(label_df, convertLabels = "character", convertVariables = "a"),
               data.frame(a = c("one", "two"),
                          b = c(2, 1), stringsAsFactors = FALSE))
  expect_equal(extractData(label_df, convertLabels = "factor", convertVariables = "a"),
               data.frame(a = c("one", "two"),
                          b = c(2, 1), stringsAsFactors = TRUE))
})

test_that("Extract data trend GADS", {
  # trend_gads <- getTrendGADS(filePaths = c("tests/testthat/helper_dataBase.db", "tests/testthat/helper_dataBase_uniqueVar.db"), years = c(2012, 2018), fast = FALSE)
  trend_gads <- suppressWarnings(getTrendGADS(filePaths = c("helper_dataBase.db", "helper_dataBase_uniqueVar.db"),
                                              years = c(2012, 2018), fast = FALSE, verbose = FALSE))
  out <- extractData(trend_gads)
  expect_equal(dim(out), c(6, 5))
  expect_equal(names(out), c("ID1", "V1", "V2", "V3", "year"))
  comp <- c(rep(2012, 3), c(rep(2018, 3)))
  attr(comp, "label") <- "Trendvariable, indicating the year of the assessment"
  expect_equal(out$year, comp)

  ## convertVariables if some variables are not in both GADS
  out2 <- extractData(trend_gads, convertVariables = "V3")
  expect_equal(out, out2)
})


test_that("Extract data trend GADS 3 MPs", {
  fp1 <- system.file("extdata", "trend_gads_2020.db", package = "eatGADS")
  fp2 <- system.file("extdata", "trend_gads_2015.db", package = "eatGADS")
  fp3 <- system.file("extdata", "trend_gads_2010.db", package = "eatGADS")
  s <- capture_output(gads_3mp <- getTrendGADS(filePaths = c(fp1, fp2, fp3), years = c(2020, 2015, 2010), fast = FALSE, verbose = FALSE))

  out <- extractData(gads_3mp)
  expect_equal(dim(out), c(180, 10))
  expect_equal(names(out), c("idstud", "gender", "dimension", "imp", "score", "traitLevel", "failMin", "passReg", "passOpt", "year"))
  expect_equal(as.numeric(out$year), c(rep(2020, 60), rep(2015, 60), rep(2010, 60)))
})


### with linking errors
test_that("with linking errors present", {
  # out <- getTrendGADSOld(filePath1 = "tests/testthat/helper_comp.db", filePath2 = "tests/testthat/helper_comp2.db", years = c(2012, 2018), lePath = "tests/testthat/helper_le.db", fast = FALSE, vSelect = c("ID", "PV"))
  out <- getTrendGADSOld(filePath1 = "helper_comp.db", filePath2 = "helper_comp2.db", years = c(2012, 2018),
                         lePath = "helper_le.db", fast = control_caching, vSelect = c("ID", "PV"))
  expect_error(dat <- extractData(out),
               "Linking errors are no longer supported by extractData. Use extractDataOld() instead.", fixed = TRUE)
})








