

facs_df <- data.frame(id = 1:3, fac1 = c("Eng", "Aus", "Ger"), fac2 = c("Ger", "Franz", "Ita"),
                      fac3 = c("Kor", "missing - omitted", "Alg"), stringsAsFactors = TRUE)
facs_gads <- import_DF(facs_df)
facs_gads <- recodeGADS(facs_gads, varName = "fac3", oldValues = 3, newValues = -99)

test_that("Errors", {
  expect_error(assimilateValLabels(facs_gads, varNames = c("fac1", "fac2"), lookup = mtcars),
               "Lookup argument is currently not supported.")
})

test_that("Assimilate 2 variables", {
  out <- assimilateValLabels(facs_gads, varNames = c("fac1", "fac2"))
  expect_equal(out$dat$fac2, c(4, 3, 5))
})

test_that("Assimilate 3 variables with missing", {
  out <- assimilateValLabels(facs_gads, varNames = c("fac1", "fac2", "fac3"))
  expect_equal(out$dat$fac2, c(5, 4, 6))
  expect_equal(out$dat$fac3, c(7, 8, 1))

})
