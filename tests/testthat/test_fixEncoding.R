


dat <- data.frame(v1 = c("kl", "mw", "hi"),
                  v2 = 1:3,
                  v3 = c("\001", "C\026", "C6"))
g <- import_DF(dat)
g <- changeVarLabels(g, varName = "v2", varLabel = c("This is a problem\001"))
g <- changeValLabels(g, varName = "v2", value = 1, valLabel = c("This is a problemC...\023"))

test_that("other with character vectors", {
  out <- fixEncoding(c("Kr\U00E4_ftigung", "Spa\U00DF"))
  expect_equal(out, c("Krae_ftigung", "Spass"))
})

# insert for manuel checks
# test_that("other with character vectors 2", {
#   out <- fixEncoding(c("Krftigung", "Spa"))
#   expect_equal(out, c("Kraeftigung", "Spass"))
# })

test_that("ASCII with character vectors", {
  out <- fixEncoding(c("C..6", "ABC", "C\037"), input = "ASCII")
  expect_equal(out, c("oe", "ABC", "ss"))
})

test_that("ASCII with GADS", {
  out <- fixEncoding(g, input = "ASCII")
  expect_equal(out$dat$v1, c("kl", "mw", "hi"))
  expect_equal(out$dat$v2, 1:3)
  expect_equal(out$dat$v3, c("", "Oe", "oe"))
})

test_that("ASCII with changes in namesGADS", {
  g2 <- changeVarNames(g, oldNames = "v3", newNames = "vC6")
  out <- fixEncoding(g2, input = "ASCII")
  expect_equal(namesGADS(out), c("v1", "v2", "voe"))
})
