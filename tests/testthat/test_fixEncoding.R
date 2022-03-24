dat <- data.frame(v1 = c("kl", "mw", "hi"),
                  v2 = 1:3,
                  v3 = c("\001", "C\026", "C6"))
g <- import_DF(dat)
g <- changeVarLabels(g, varName = "v2", varLabel = c("This is a problem\001"))
g <- changeValLabels(g, varName = "v2", value = 1, valLabel = c("This is a problemC...\023"))

test_that("with character vectors", {
  out <- fixEncoding(c("C..6", "ABC", "C\037"))
  expect_equal(out, c("oe", "ABC", "ss"))
})

test_that("with GADS", {
  out <- fixEncoding(g)
  expect_equal(out$dat$v1, c("kl", "mw", "hi"))
  expect_equal(out$dat$v2, 1:3)
  expect_equal(out$dat$v3, c("", "Oe", "oe"))
})

test_that("with changes in namesGADS", {
  g2 <- changeVarNames(g, oldNames = "v3", newNames = "vC6")
  out <- fixEncoding(g2)
  expect_equal(namesGADS(out), c("v1", "v2", "voe"))
})
