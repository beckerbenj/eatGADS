g <- import_raw(df = data.frame(var1 = c("ab c","bb"), var2 = c(1,NaN), var3 = c(1.01, 2.00), var4 = c(1.0001, 4.00187243564195786431587643596),var5 = c("ab c","b\"b"),var6 = c("ab c","b\"b"),var7=NA,stringsAsFactors = FALSE),
                varLabels = data.frame(varName = c("var1", "var2", "var3", "var4","var5","var6","var7"), varLabel = c("a label", NA, "another label", NA, NA, NA,NA), stringsAsFactors = FALSE),
                valLabels = data.frame(varName = c("var1", "var1", "var2", "var2", "var2","var3", "var3","var5","var6"), value = c(-96, -99, -99, 0, 1,-96, -99, -9,-9),
                                       valLabel = c("miss1", "miss2", "miss2","right","wrong","miss1", "miss2", NA, NA),
                                       missings = c("miss", "miss", "miss","valid","valid","miss", "miss", "miss", "miss"), stringsAsFactors = FALSE))


f_txt <- tempfile(fileext = ".txt")
f_sps <- gsub("txt$", "sps", f_txt)

test_that("writeData", {
  expect_message(write_spss2(g, txtPath = f_txt, dec=","), "Format and data for variable 'var7' are NA. Format cannot be derived from data and will be set to A1.", fixed=TRUE)
})

syntax <- readChar(f_sps, file.info(f_sps)$size)


test_that("Format and writeHeader", {

  expect_true(grepl("PRESERVE.", syntax))
  expect_true(grepl("SET DECIMAL", syntax))
  expect_true(grepl("GET DATA", syntax))
  expect_true(grepl("TYPE=TXT", syntax))
  expect_true(grepl("DELCASE=LINE", syntax))
  expect_true(grepl("DELIMITERS=\";\"", syntax))
  expect_true(grepl("QUALIFIER='\"'", syntax))
  expect_true(grepl("ARRANGEMENT=DELIMITED", syntax))
  expect_true(grepl("FIRSTCASE=1", syntax))
  expect_true(grepl("DATATYPEMIN PERCENTAGE=95.0", syntax))
  expect_true(grepl("VARIABLES=", syntax))
  expect_true(grepl("EXECUTE.", syntax))

  r1 <- list()
  r1$labels <- g$labels
  stopifnot(identical(unique(r1$labels$varName),names(g$dat)))
  r1$varInfo <- unique(r1$labels[, c("varName", "varLabel", "format")])
  r1$valInfo <- unique(r1$labels[which(!is.na(r1$labels$value)), c("varName", "value", "valLabel", "missings")])
  r1$misInfo <- unique(r1$labels[which(!is.na(r1$labels$value) & r1$labels$missings == "miss"), c("varName", "value", "valLabel", "missings")])
  r1$chv <- sapply(g$dat, is.character)

  expect_true(grepl("var1 A4", syntax))
  expect_true(grepl("var2 F2", syntax))
  expect_true(grepl("var3 F4.2", syntax))
  expect_true(grepl("var4 F16.14", syntax))
  expect_true(grepl("var5 A4", syntax))
  expect_true(grepl("var6 A4", syntax))
  expect_warning(x1 <- writeHeader(r1, f_txt, f_sps, dec=",", fileEncoding="UTF-8"), "Format statement still contains 'NA' values, SPSS syntax will probably not work. Consider changeFormat=TRUE.", fixed=TRUE)
})

test_that("writeData", {
  expect_message(g1<-writeData(g, txtPath = f_txt, dec=",", fileEncoding = "UTF-8"),"In character variable(s) var5, var6 quotation marks (\") had to be replaced with inverted commas (').", fixed=TRUE)
  expect_equal(g1$dat, data.frame(var1 = c("ab c","bb"), var2 = c(1,NaN), var3 = c(1.01, 2.00), var4 = c(1.0001, 4.00187243564195786431587643596),var5 = c("ab c","b'b"),var6 = c("ab c","b'b"),var7=NA,stringsAsFactors = FALSE))
  expect_equal(read.csv2(f_txt, header=FALSE), data.frame(V1 = c("ab c","bb"), V2 = c(1,NaN), V3 = c(1.01, 2.00), V4 = c(1.0001, 4.00187243564195786431587643596),V5 = c("ab c","b'b"),V6 = c("ab c","b'b"),V7=NA,stringsAsFactors = FALSE))
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
  expect_true(grepl("var1 \\('-99','-96'\\)", syntax))
  expect_true(grepl("var2 \\(-99\\)", syntax))
  expect_true(grepl("var3 \\(-99,-96\\)", syntax))
})

