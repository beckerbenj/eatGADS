

facs_df <- data.frame(id = 1:3, fac1 = c("Eng", "Aus", "Ger"), fac2 = c("Ger", "Franz", "Ita"),
                      fac3 = c("Kor", "missing - omitted", "Alg"), stringsAsFactors = TRUE)
facs_gads <- import_DF(facs_df)

facs_gads1 <- facs_gads
facs_gads1 <- recodeGADS(facs_gads1, varName = "fac3", oldValues = 3, newValues = -99)
for(i in paste0("fac", 1:3)) {
  facs_gads1 <- changeMissings(facs_gads1, varName = i, value = -99, missings = "miss")
}
facs_gads2 <- facs_gads1
for(i in paste0("fac", 1:3)) {
  facs_gads2 <- changeValLabels(facs_gads2, varName = i, value = -99, valLabel = "missing - omitted")
}


test_that("resize_vector", {
  out <- resize_vector(c(1:5), 6)
  expect_equal(out, data.frame(V1 = c(1:5, 1)))

  out <- resize_vector(c(1:5), 3)
  expect_equal(out[, 1], 1:3)
  expect_equal(out[, 2], c(4, 5, 1))

  out <- resize_vector(letters[1:5], 2)
  expect_equal(out[, 1], c("a", "b"))
  expect_equal(out[, 2], c("c", "d"))
  expect_equal(out[, 3], c("e", "a"))
})

test_that("Errors", {
  expect_error(assimilateValLabels(facs_gads1, varNames = c("fac1", "fac2"), lookup = mtcars),
                "Lookup argument is currently not supported.")
  expect_error(assimilateValLabels(facs_gads1, varNames = paste0("fac", 1:3)),
               "Missing values in 'valLabel' for declared missings.")
})

test_that("Assimilate 2 variables", {
  out <- assimilateValLabels(facs_gads, varNames = c("fac1", "fac2"))
  expect_equal(out$dat$fac2, c(4, 3, 5))
})


test_that("Assimilate 3 variables with missing", {
  out <- assimilateValLabels(facs_gads2, varNames = c("fac1", "fac2", "fac3"))
  expect_equal(out$dat$fac2, c(5, 4, 6))
  expect_equal(out$dat$fac3, c(7, -99, 1))
  expect_equal(extractMeta(out, "fac3")[1, "valLabel"], "missing - omitted")
})


test_that("Assimilate 2 variables with not used missing", {
  out <- assimilateValLabels(facs_gads2, varNames = c("fac1", "fac2"))
  expect_equal(out$dat$fac2, c(4, 3, 5))
  expect_equal(extractMeta(out, "fac1")[1, "valLabel"], "missing - omitted")
})
