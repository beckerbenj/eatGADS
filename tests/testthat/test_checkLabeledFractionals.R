# load + generally prepare data
dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))
df1 <- changeValLabels(GADSdat = dfSAV,
                       varName = "VAR1",
                       value = 2,
                       valLabel = "Apple")

# general output structure to be changed for each test
outlist <- data.frame(varName = NA_character_,
                      value = NA_real_,
                      missings = NA_character_,
                      empty = NA)[0,]

test_that("Correctly identify metadata without labeled fractional values", {
  check_result <- checkLabeledFractionals(GADSdat = df1)
  expect_equal(check_result, outlist)
})

test_that("One existing labeled fractional in one variable", {
  df2 <- suppressMessages(removeVars(GADSdat = df1,
                                     vars = c("VAR2", "VAR3")))
  df2 <- recodeGADS(GADSdat = df2,
                    varName = "VAR1",
                    oldValues = 1,
                    newValues = .5,
                    existingMeta = "ignore")
  check_result <- checkLabeledFractionals(GADSdat = df2)
  outlist2 <- outlist
  outlist2[1,] <- list("VAR1", .5, "valid", FALSE)
  expect_equal(check_result, outlist2)
})

test_that("Only empty labeled fractionals", {
  df2 <- changeValLabels(GADSdat = df1,
                         varName = "VAR1",
                         value = c(.2, -94.94),
                         valLabel = c("point two", "mbd"))
  df2 <- changeMissings(GADSdat = df2,
                        varName = "VAR1",
                        value = c(.2, -94.94),
                        missings = c("valid", "miss"))
  check_result <- checkLabeledFractionals(GADSdat = df2)
  outlist2 <- outlist
  outlist2[1,] <- list("VAR1", -94.94, "miss", TRUE)
  outlist2[2,] <- list("VAR1", .2, "valid", TRUE)
  expect_equal(check_result, outlist2)
})

# remove this last one?
test_that("Two labeled fractionals in the same variable where one value is empty", {
  df2 <- recodeGADS(GADSdat = df1,
                    varName = "VAR1",
                    oldValues = 2,
                    newValues = .5555555,
                    existingMeta = "ignore")
  df2 <- changeValLabels(GADSdat = df2,
                         varName = "VAR1",
                         value = .000001,
                         valLabel = "nothing")
  check_result <- checkLabeledFractionals(GADSdat = df2)
  outlist2 <- outlist
  outlist2[1,] <- list("VAR1", .000001, "valid", TRUE)
  outlist2[2,] <- list("VAR1", .5555555, "valid", FALSE)
  expect_equal(check_result, outlist2)
})
