g <- import_raw(df = data.frame(var1 = c("ab c","bb"), var2 = c(1,NaN), var3 = c(1.01, 2.00), var4 = c(1.0001, 4.001),stringsAsFactors = FALSE),
                varLabels = data.frame(varName = c("var1", "var2", "var3", "var4"), varLabel = c("a label", NA, "another label", NA), stringsAsFactors = FALSE),
                valLabels = data.frame(varName = c("var1", "var1", "var2", "var2", "var2","var3", "var3"), value = c(-96, -99, -99, 0, 1,-96, -99),
                                       valLabel = c("miss1", "miss2", "miss2","right","wrong","miss1", "miss2"),
                                       missings = c("miss", "miss", "miss","valid","valid","miss", "miss"), stringsAsFactors = FALSE))
g <- changeSPSSformat(g, varName = "var1", format = "A3")
f_txt <- tempfile(fileext = ".txt")
f_sps <- tempfile(fileext = ".sps")
write_spss2(g, filePath = f_txt, syntaxPath = f_sps, dec=",")

out <- readMultisep(f_txt, ";;;")
syntax <- readChar(f_sps, file.info(f_sps)$size)

test_that("Write spss 2 overall", {

  expect_equal(out, data.frame(X1 = c("ab c","bb"), X2 = c(1,NA), X3 = c("1,01", "2"), X4 = c("1,0001", "4,001"), X5=c(1,1),stringsAsFactors = FALSE))
  expect_true(grepl("EXECUTE.", syntax))
  expect_true(grepl("DELETE VARIABLES xxxtgw.", syntax))
})




test_that("writeData", {
  g1<-writeData(g, filePath = f_txt, dec=",", fileEncoding = "UTF-8")

  expect_true(identical(g1$dat, data.frame(var1= c("ab c", "bb"), var2=c(1, NaN), var3=c(1.01,2.00), var4=c(1.0001, 4.0010), xxxtgw=c(1,1))))
  expect_true(setequal(g1$labels[9,],  data.frame(varName=c("xxxtgw","a"),varLabel=c(NA,"a"),format=c(NA,"a"),display_width=c(NaN,1),labeled=c("no","no"),value=c(NaN,1),valLabel=c(NA,"a"),missings=c(NA,"a"),stringsAsFactors = FALSE)[1,]))

})



test_that("createInputWriteFunctions", {
   r1<-createInputWriteFunctions(g)

  expect_true(identical(r1$labels$varName, c("var1", "var1", "var2", "var2", "var2", "var3", "var3", "var4")))
  expect_true(identical(r1$labels$format, as.character(c("A3","A3",rep(NA,6)))))
  expect_true(identical(r1$varInfo$varName, c("var1", "var2", "var3", "var4")))
  expect_true(identical(r1$valInfo$varName, c("var1", "var1", "var2", "var2", "var2", "var3", "var3")))
  expect_true(identical(r1$misInfo$varName, c("var1", "var1", "var2", "var3", "var3")))
  expect_true(identical(as.numeric(r1$lengths), c(4,2,2,1)))
  expect_true(identical(r1$dl.varnames, c("var1 (A4)", "var2 (F2)", "var3 (F4.2)", "var4 (F6.4)")))
  expect_true(identical(unname(r1$chv), c(TRUE, FALSE, FALSE, FALSE)))
})




test_that("writeHeader", {
  expect_true(grepl("DATA LIST FILE=.", syntax))
  expect_true(grepl("free", syntax))
  expect_true(grepl("var1 \\(A4\\) var2 \\(F2\\) var3 \\(F4.2\\) var4 \\(F6.4\\)", syntax))
})



test_that("writeVaLab", {
  expect_true(grepl("VARIABLE LABELS", syntax))
  expect_true(grepl("VALUE LABELS", syntax))
  expect_true(grepl("var3 \"another label\"", syntax))
  expect_true(grepl("var1", syntax))
  expect_true(grepl("-99 \"miss2\"", syntax))
  expect_true(grepl("1 \"wrong\"", syntax))
})


test_that("writeMisCode", {
  expect_true(grepl("MISSING VALUES", syntax))
  expect_true(grepl("var1 \\('-99,-96'\\)", syntax))
  expect_true(grepl("var2 \\(-99\\)", syntax))
  expect_true(grepl("var3 \\(-99,-96\\)", syntax))
})

