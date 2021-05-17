

test_that("Write spss 2", {
  g <- import_raw(df = data.frame(var1 = c("ab c","bb"), var2 = c(1,0), var3 = c(1.01, 2.00), var4 = c(1.0001, 4.001),stringsAsFactors = FALSE),
                  varLabels = data.frame(varName = c("var1", "var2", "var3", "var4"), varLabel = c("a label", NA, "another label", NA), stringsAsFactors = FALSE),
                  valLabels = data.frame(varName = c("var1", "var1", "var2", "var2", "var2","var3", "var3"), value = c(-96, -99, -99, 0, 1,-96, -99),
                                         valLabel = c("miss1", "miss2", "miss2","right","wrong","miss1", "miss2"),
                                         missings = c("miss", "miss", "miss","valid","valid","miss", "miss"), stringsAsFactors = FALSE))
  g <- changeSPSSformat(g, varName = "var1", format = "A3")
  f_txt <- tempfile(fileext = ".txt")
  f_sps <- tempfile(fileext = ".sps")
  write_spss2(g, filePath = f_txt, syntaxPath = f_sps, dec=",")

  # out <- read.table(f_txt, stringsAsFactors = FALSE)
  out <- readMultisep(f_txt, ";;;")
  expect_equal(out, data.frame(X1 = c("ab c","bb"), X2 = c(1,0), X3 = c("1,01", "2"), X4 = c("1,0001", "4,001"),stringsAsFactors = FALSE))

  syntax <- readChar(f_sps, file.info(f_sps)$size)
  expect_true(grepl("EXECUTE.", syntax))
  expect_true(grepl("^DATA LIST FILE", syntax))
  expect_true(grepl("VARIABLE LABELS", syntax))
  expect_true(grepl("VALUE LABELS", syntax))
  expect_true(grepl("MISSING VALUES", syntax))
})
