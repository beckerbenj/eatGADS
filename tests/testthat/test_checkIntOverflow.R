# load + generally prepare data
dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))
df1 <- changeValLabels(GADSdat = dfSAV,
                       varName = "VAR1",
                       value = 2,
                       valLabel = "Apple")

# general output structure to be changed for each test
outlist <- data.frame(varName = character(),
                      value = numeric(),
                      missings = character(),
                      empty = logical(),
                      rownum = integer())

test_that("Correctly identify metadata without very large labeled values", {
  check_result <- checkIntOverflow(GADSdat = df1)
  expect_equal(check_result, outlist)
})

test_that("One very large value without any empty labels in the data set", {
  df2 <- suppressMessages(removeVars(GADSdat = df1,
                                     vars = c("VAR2", "VAR3")))
  df2 <- recodeGADS(GADSdat = df2,
                    varName = "VAR1",
                    oldValues = 1,
                    newValues = (2*10^15))
  check_result <- checkIntOverflow(GADSdat = df2)
  outlist2 <- outlist
  outlist2[1,] <- list("VAR1", (2*10^15), "valid", FALSE, 4)
  expect_equal(check_result, outlist2)
})

test_that("Two existing labeled value exactly 1 above/below the limit in one variable", {
  df2 <- recodeGADS(GADSdat = df1,
                    varName = "VAR1",
                    oldValues = c(1,-99),
                    newValues = c(2147483648, -2147483648),
                    existingMeta = "ignore")
  check_result <- checkIntOverflow(GADSdat = df2)
  outlist2 <- outlist
  outlist2[1,] <- list("VAR1", -2147483648, "miss", FALSE, 1)
  outlist2[2,] <- list("VAR1", 2147483648, "valid", FALSE, 4)
  expect_equal(check_result, outlist2)
})

test_that("Only empty very large labeled values", {
  df2 <- changeValLabels(GADSdat = df1,
                         varName = "VAR1",
                         value = c(-9999999999, 9999999999),
                         valLabel = c("mbddd", "other"))
  df2 <- changeMissings(GADSdat = df2,
                        varName = "VAR1",
                        value = c(-9999999999, 9999999999),
                        missings = c("miss", "valid"))
  check_result <- checkIntOverflow(GADSdat = df2)
  outlist2 <- outlist
  outlist2[1,] <- list("VAR1", -9999999999, "miss", TRUE, 1)
  outlist2[2,] <- list("VAR1", 9999999999, "valid", TRUE, 6)
  expect_equal(check_result, outlist2)
})

test_that("Detect only very large whole-number values but not fractional values", {
  # Fractional values would be detected by checkLabeledFractionals
  df2 <- recodeGADS(GADSdat = df1,
                    varName = "VAR1",
                    oldValues = c(1,-99),
                    newValues = c(2147483648, -9999999999.99),
                    existingMeta = "ignore")
  check_result <- checkIntOverflow(GADSdat = df2)
  outlist2 <- outlist
  outlist2[1,] <- list("VAR1", 2147483648, "valid", FALSE, 4)
  expect_equal(check_result, outlist2)
})
