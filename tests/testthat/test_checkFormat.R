
g <- import_raw(df = data.frame(var1 = c("ab c","bb"),
                                var2 = c(1,NaN),
                                var3 = c(1.01, 2.00),
                                var4 = c(1.0001, 4.00187243564195786431587643596),
                                var5=c("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore","aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata ähh öh üh"),
                                stringsAsFactors = FALSE),
                                varLabels = data.frame(varName = c("var1", "var2", "var3", "var4", "var5"),
                                                       varLabel = c("a label", NA, "another label", NA, "lorem ipsum"),
                                                       stringsAsFactors = FALSE),
                                valLabels = data.frame(varName = c("var1", "var1", "var2", "var2", "var2","var3", "var3","var5"),
                                                       value = c(-96, -99, -99, 0, 1,-96, -99,-99),
                                                       valLabel = c("miss1", "miss2", "miss2","right","wrong","miss1", "miss2", "missing"),
                                                       missings = c("miss", "miss", "miss","valid","valid","miss", "miss", "miss"),
                                                       stringsAsFactors = FALSE))


test_that("All formats were correctly set", {
  g1 <- checkFormat(g)
  expect_equal(g1$labels$format, c("A4", "A4", "F2", "F2", "F2", "F4.2", "F4.2", "F16.14", "A146"))
})


test_that("All formats were correctly set", {
  g2 <- checkFormat(g, type="normal")
  expect_equal(g2$labels$format, c("A4", "A4", "F2", "F2", "F2", "F4.2", "F4.2", "F16.14", "A143"))
})

p <- extractVars(pisa, c("idstud","ma_pv1"))

test_that("Rounding condition", {
  p1 <- checkFormat(p)
  p2 <- checkFormat(p, type="other")

  expect_equal(p1$dat$ma_pv1,round(p1$dat$ma_pv1,16))
  expect_message(p3 <- checkFormat(p), "Variable ma_pv1 has more decimals than SPSS allows (18) and will be rounded to 16 decimal places.", fixed=TRUE)
  expect_equal(p2$dat$ma_pv1,round(p2$dat$ma_pv1,18))

  # No Character Variables
  expect_true(all(grepl("^F",p1$labels$format)))
})


p <- extractVars(pisa, c("idstud"))
test_that("No Change", {
  expect_message(p3 <- checkFormat(p, changeFormat = FALSE), "Format mismatch for Variable idstud: F8.0 vs. F3", fixed=TRUE)
})


test_that("Empty variables", {
  # preparation
  suppressMessages(g3 <- g2 <- g1 <- checkFormat(g))

  g1$dat$var1 <- NA_integer_
  expect_message(out1 <- checkFormat(g1), "Format of Variable var1 will be changed from A4 to A3", fixed=TRUE)
  expect_equal(out1$labels$format, c("A3", "A3", "F2", "F2", "F2", "F4.2", "F4.2", "F16.14", "A146"))
  expect_true(is.character(out1$dat$var1))

  g2$dat$var1 <- NA
  expect_message(out2 <- checkFormat(g2), "Format of Variable var1 will be changed from A4 to A3", fixed=TRUE)
  expect_equal(out2$labels$format, c("A3", "A3", "F2", "F2", "F2", "F4.2", "F4.2", "F16.14", "A146"))
  expect_true(is.character(out2$dat$var1))

  g3$dat$var1 <- NA
  g3 <- removeValLabels(g3, varName = "var1", value = c(-99, -96))
  expect_silent(out3 <- checkFormat(g3))
  expect_equal(out3$labels$format, c("A4", "F2", "F2", "F2", "F4.2", "F4.2", "F16.14", "A146"))
  expect_true(is.character(out3$dat$var1))
})
