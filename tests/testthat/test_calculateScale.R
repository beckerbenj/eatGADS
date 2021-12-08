
# testM <- import_spss("tests/testthat/helper_spss_missings.sav")
testM <- import_spss("helper_spss_missings.sav")

test_that("errors", {
  expect_error(calculateScale(testM, items = "VAR1", scale = "VARnew"),
               "'items' needs to be a character vector of at least length 2.")
  expect_error(calculateScale(testM, items = c("VAR1", "VAR2"), scale = c("VARnew", "VARnew2")),
               "'scale' needs to be a character vector of length 1.")
})


dat <- data.frame(ID = 1:4,
                  it1 = c(3, 3, 4, 2),
                  it2 = c(4, 4, NA, 3),
                  it3 = c(2, 3, NA, NA),
                  other = c(1, 3, 1, NA))
gads <- import_DF(dat)

dat_nomiss <- data.frame(ID = 1:4,
                  it1 = c(3, 3, 4, 2),
                  it2 = c(4, 4, 3, 3),
                  it3 = c(2, 3, 2, 1),
                  other = c(1, 3, 1, NA))
gads_nomiss <- import_DF(dat_nomiss)

test_that("normal functionality", {
  out <- calculateScale(gads, items = paste0("it", 1:3), scale = "scaleNew")
  expect_equal(namesGADS(out)[5], "scaleNew")
  expect_equal(round(out$dat$scaleNew, 2), c(3, 3.33, 4, 2.5))

  out2 <- calculateScale(gads, items = paste0("it", 1:3), scale = "scaleNew", maxNA = 1)
  expect_equal(round(out2$dat$scaleNew, 2), c(3, 3.33, NA, 2.5))
})

test_that("reporting descriptives", {
  cons <- capture_output(out <- calculateScale(gads, items = paste0("it", 1:3), scale = "scaleNew", reportDescr = TRUE))
  expect_true(grepl("4.00    0.00 $", cons))

  cons2 <- capture_output(out2 <- calculateScale(gads, items = paste0("it", 1:3), scale = "scaleNew", maxNA = 1, reportDescr = TRUE))
  expect_true(grepl("1.00    0.00 $", cons2))
})

## compare without missings
# cronbach_alpha(dat_nomiss[, paste0("it", 1:3)])
# psych::alpha(dat_nomiss[, paste0("it", 1:3)])
# psy::cronbach(dat_nomiss[, paste0("it", 1:3)])
# ltm::cronbach.alpha(dat_nomiss[, paste0("it", 1:3)])
#
# ## compare with missings
# cronbach_alpha(dat[, paste0("it", 1:3)])
# psych::alpha(dat[, paste0("it", 1:3)], na.rm = TRUE, use = "complete.obs")
# psy::cronbach(dat[, paste0("it", 1:3)])
# ltm::cronbach.alpha(dat[, paste0("it", 1:3)], na.rm = TRUE)


test_that("cronbach alpha", {
  out <- cronbach_alpha(dat_nomiss[, paste0("it", 1:3)])
  expect_equal(round(out, 2), 0.67)

  out2 <- cronbach_alpha(dat[, paste0("it", 1:3)])
  expect_equal(out2, 0)
})
