

test_that("Write spss 2", {
  g <- import_raw(df = data.frame(var1 = "abc", var2 = 1, stringsAsFactors = FALSE),
                  varLabels = data.frame(varName = c("var1", "var2"), varLabel = c("a label", NA), stringsAsFactors = FALSE),
                  valLabels = data.frame(varName = c("var1", "var1", "var2"), value = c(-96, -99, -99),
                                         valLabel = c("miss1", "miss2", "miss1"),
                                         missings = c("miss", "miss", "miss"), stringsAsFactors = FALSE))
  g <- changeSPSSformat(g, varName = "var1", format = "A3")
  f_txt <- tempfile(fileext = ".txt")
  f_sps <- tempfile(fileext = ".sps")
  write_spss2(g, filePath = f_txt, syntaxPath = f_sps)

  out <- read.table(f_txt, stringsAsFactors = FALSE)
  expect_equal(as.character(out), as.character(g$dat))
})
