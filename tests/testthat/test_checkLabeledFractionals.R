# load + generally prepare data
dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))
df1 <- changeValLabels(dfSAV, "VAR1", 2, "Apple")

# general output structure to be changed for each test
outlist <- data.frame(varName = "<none found>",
                      value = NA_real_,
                      missings = NA_character_,
                      empty = NA)

test_that("Correctly identify metadata without labeled fractional values", {
  check_result <- checkLabeledFractionals(df1)
  expect_equal(check_result, outlist)
})

test_that("One existing labeled fractional in one variable", {
  df2 <- recodeGADS(df1, "VAR1", 1, .5, "ignore")
  check_result <- checkLabeledFractionals(df2)
  outlist2 <- outlist
  outlist2[1,] <- list("VAR1", .5, "valid", FALSE)
  expect_equal(check_result, outlist2)
})

test_that("Only empty labeled fractionals", {
  df2 <- changeValLabels(df1, "VAR1", c(.2, -94.94), c("point two", "mbd"))
  df2 <- changeMissings(df2, "VAR1", c(.2, -94.94), c("valid", "miss"))
  check_result <- checkLabeledFractionals(df2)
  outlist2 <- outlist
  outlist2[1,] <- list("VAR1", -94.94, "miss", TRUE)
  outlist2[2,] <- list("VAR1", .2, "valid", TRUE)
  expect_equal(check_result, outlist2)
})

# remove this last one?
test_that("Two labeled fractionals in the same variable where one value is empty", {
  df2 <- recodeGADS(df1, "VAR1", 2, .5555555, "ignore")
  df2 <- changeValLabels(df2, "VAR1", .000001, "nothing")
  check_result <- checkLabeledFractionals(df2)
  outlist2 <- outlist
  outlist2[1,] <- list("VAR1", .000001, "valid", TRUE)
  outlist2[2,] <- list("VAR1", .5555555, "valid", FALSE)
  expect_equal(check_result, outlist2)
})
