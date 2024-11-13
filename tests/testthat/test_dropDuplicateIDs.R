

gads_ori <- import_DF(data.frame(id_var = c(1, 2, 5, 4, 4, 5),
                     var1 = c(1, 2, 1, -99, -99, -99),
                     var2 = c("a", "a", "b", "-99", "-99", "-99"),
                     var3 = c(2, 2, -99, 3, -99, 1)))
for(single_var in c("var1", "var2", "var3")) {
  gads_ori <- changeMissings(gads_ori, varName = single_var, value = -99, missings = "miss")
}

test_that("Drop duplicate IDs according to missings on all variables", {
  out <- dropDuplicateIDs(gads_ori, ID = "id_var")

  expect_equal(nrow(out$dat), 4)
  expect_equal(out$dat$id_var, c(1, 2, 5, 4))
  expect_equal(out$dat$var1, c(1, 2, 1, -99))
  expect_equal(out$dat$var2, c("a", "a", "b", "-99"))
  expect_equal(out$dat$var3, c(2, 2, -99, 3))
})

test_that("drop duplicate IDs according to missings on a specific variable", {
  out <- dropDuplicateIDs(gads_ori, ID = "id_var", varNames = "var3")

  expect_equal(nrow(out$dat), 4)
  expect_equal(out$dat$id_var, c(1, 2, 4, 5))
  expect_equal(out$dat$var1, c(1, 2, -99, -99))
  expect_equal(out$dat$var2, c("a", "a", "-99", "-99"))
  expect_equal(out$dat$var3, c(2, 2, 3, 1))
})

test_that("Drop duplicate IDs with cases with identical missing numbers", {
  gads_ori$dat$var3[4] <- -99
  gads_ori$dat$var1[6] <- 1
  warns <- capture_warnings(out <- dropDuplicateIDs(gads_ori, ID = "id_var"))

  expect_equal(warns[1],
               "Multiple rows with id_var 4 have the same number of missing values (3).")
  expect_equal(warns[2],
               "Multiple rows with id_var 5 have the same number of missing values (1).")

})
