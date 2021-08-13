
g <- import_raw(df = data.frame(var1 = c("ab c","bb"), var2 = c(1,NaN), var3 = c(1.01, 2.00), var4 = c(1.0001, 4.00187243564195786431587643596),stringsAsFactors = FALSE),
                                varLabels = data.frame(varName = c("var1", "var2", "var3", "var4"), varLabel = c("a label", NA, "another label", NA), stringsAsFactors = FALSE),
                                valLabels = data.frame(varName = c("var1", "var1", "var2", "var2", "var2","var3", "var3"), value = c(-96, -99, -99, 0, 1,-96, -99),
                                                       valLabel = c("miss1", "miss2", "miss2","right","wrong","miss1", "miss2"),
                                                       missings = c("miss", "miss", "miss","valid","valid","miss", "miss"), stringsAsFactors = FALSE))


g1 <- checkFormat(g)

test_that("All formats were correctly set", {
  expect_equal(g1$labels$format, c("A4", "A4", "F2", "F2", "F2", "F4.2", "F4.2", "F16.14"))
})
