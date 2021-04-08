

test_that("Write spss 2", {
  g <- import_raw(df = data.frame(var1 = c("abc","bb"), var2 = c(1,0), var3 = c(1.01, 2.00), var4 = c(1.0001, 4.001),stringsAsFactors = FALSE),
                  varLabels = data.frame(varName = c("var1", "var2", "var3", "var4"), varLabel = c("a label", NA, "another label", NA), stringsAsFactors = FALSE),
                  valLabels = data.frame(varName = c("var1", "var1", "var2", "var2", "var2","var3", "var3"), value = c(-96, -99, -99, 0, 1,-96, -99),
                                         valLabel = c("miss1", "miss2", "miss2","right","wrong","miss1", "miss2"),
                                         missings = c("miss", "miss", "miss","valid","valid","miss", "miss"), stringsAsFactors = FALSE))
  g <- changeSPSSformat(g, varName = "var1", format = "A3")
  f_txt <- tempfile(fileext = ".txt")
  f_sps <- tempfile(fileext = ".sps")
  write_spss2(g, filePath = f_txt, syntaxPath = f_sps,dec=",")

  out <- read.table(f_txt, stringsAsFactors = FALSE)
  expect_equal(out, data.frame(V1 = c("abc","bb"), V2 = c(1,0), V3 = c("1,01", "2"), V4 = c("1,0001", "4,001"),stringsAsFactors = FALSE))
})
