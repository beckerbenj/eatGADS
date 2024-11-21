# load + generally prepare data
dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))
df1 <- changeValLabels(dfSAV, "VAR1", 2, "Apple")

# general output structure to be changed for each test
outlist <- lapply(1:2, function(x) x = data.frame(varName = "<none found>",
                                                  value = NA_real_,
                                                  empty = NA))
names(outlist) <- c("valid", "miss")

test_that("Correctly identify metadata without labeled fractional values", {
  checkres <- checkLabeledFractionals(df1)
  expect_equal(checkres, outlist)
  # for (subset in c("valid", "miss")) {
  #   expect_equal(checkres[[subset]]$varName, "<none found>")
  #   expect_equal(checkres[[subset]]$value, NA_real_)
  #   expect_equal(checkres[[subset]]$empty, NA)
  # }
})

test_that("One existing labeled fractional in one variable", {
  df2 <- recodeGADS(df1, "VAR1", 1, .5, "ignore")
  checkres <- checkLabeledFractionals(df2)
  outlist2 <- outlist
  outlist2$valid[1, ] <- list("VAR1", .5, FALSE)
  expect_equal(checkres, outlist2)
  # expect_equal(checkres$valid$varName, "VAR1")
  # expect_equal(checkres$valid$value, 0.5)
  # expect_equal(checkres$valid$empty, FALSE)
  # expect_equal(checkres$miss$varName, "<none found>")
})

test_that("Two existing labeled fractionals with different tags in one variable", {
  df2 <- recodeGADS(df1, "VAR1", c(1, -99), c(.7, -.99), "ignore")
  checkres <- checkLabeledFractionals(df2)
  outlist2 <- outlist
  outlist2$valid[1, ] <- list("VAR1", .7, FALSE)
  outlist2$miss[1, ] <- list("VAR1", -.99, FALSE)
  expect_equal(checkres, outlist2)
  # expect_equal(checkres$valid$varName, "VAR1")
  # expect_equal(checkres$valid$value, 0.7)
  # expect_equal(checkres$valid$empty, FALSE)
  # expect_equal(checkres$miss$varName, "VAR1")
  # expect_equal(checkres$miss$value, -0.99)
  # expect_equal(checkres$miss$empty, FALSE)
})

test_that("Only empty labeled fractionals", {
  df2 <- changeValLabels(df1, "VAR1", c(.2, -94.94), c("point two", "mbd"))
  df2 <- changeMissings(df2, "VAR1", c(.2, -94.94), c("valid", "miss"))
  checkres <- checkLabeledFractionals(df2)
  outlist2 <- outlist
  outlist2$valid[1, ] <- list("VAR1", .2, TRUE)
  outlist2$miss[1, ] <- list("VAR1", -94.94, TRUE)
  expect_equal(checkres, outlist2)
  # expect_equal(checkres$valid$varName, "VAR1")
  # expect_equal(checkres$valid$value, 0.2)
  # expect_equal(checkres$valid$empty, TRUE)
  # expect_equal(checkres$miss$varName, "<none found>")
})

test_that("Two labeled fractionals in the same variable where one value is empty", {
  df2 <- recodeGADS(df1, "VAR1", 2, .5555555, "ignore")
  df2 <- changeValLabels(df2, "VAR1", .000001, "nothing")
  checkres <- checkLabeledFractionals(df2)
  outlist2 <- outlist
  outlist2$valid[1, ] <- list("VAR1", .000001, TRUE)
  outlist2$valid[2, ] <- list("VAR1", .5555555, FALSE)
  expect_equal(checkres, outlist2)
})
