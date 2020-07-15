
################# Match values and variables labels ---------------------------------------------------
mt2 <- data.frame(ID = 1:4, mc1 = c(1, 0, 0, 0), mc2 = c(0, 0, 0, 0), mc3 = c(1, 1, 1, 0), text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, "Ger"),stringsAsFactors = FALSE)
mt2_gads <- import_DF(mt2)
mt3_gads <- changeVarLabels(mt2_gads, varName = c("mc1", "mc2", "mc3"), varLabel = c("Lang: Eng", "Aus spoken", "other"))
df <- data.frame(v1 = c("j", "i", NA, NA),
                 v2 = c(NA, "i", NA, "k"),
                 v3 = c("j", NA, NA, "j"), stringsAsFactors = FALSE)

test_that("Basic errors in matchValues_varLabels", {
  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc4"), values = c("Ger", "Esp")), "The following vars are not a variable in the GADSdat:\nmc4")
  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2"), values = c("Ger", "Esp"), label_by_hand = c("other" = "mc3")), "All variable names in label_by_hand must be variables in mc_vars.")
})

test_that("Match values and variable names by variable labels", {
  out <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "Eng"), label_by_hand = c("other" = "mc3"))
  expect_equal(unname(out), c("mc1", "mc2", "mc3"))
  expect_equal(names(out), c("Eng", "Aus", "other"))

  # empty matches
  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Ger", "Esp", "Eng")), "The following mc_vars have not been assigned a value: mc2, mc3")
  out2 <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Ger", "Esp", "Eng"), label_by_hand = c("other" = "mc3", "Aus" = "mc2"))
  expect_equal(unname(out2), c("mc1", "mc2", "mc3"))
  expect_equal(names(out2), c("Eng", "Aus", "other"))

  expect_error(matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Ger", "Esp")), "The following mc_vars have not been assigned a value: mc1, mc2, mc3")

  #expect_equal(out$v2, c(NA, NA, NA, "k"))
})
