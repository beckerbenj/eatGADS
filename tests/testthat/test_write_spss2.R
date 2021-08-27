g <- import_raw(df = data.frame(var1 = c("ab c","bb"), var2 = c(1,NaN), var3 = c(1.01, 2.00), var4 = c(1.0001, 4.00187243564195786431587643596),var5 = c("ab c","b\"b"),var6 = c("ab c","b\"b"),stringsAsFactors = FALSE),
                varLabels = data.frame(varName = c("var1", "var2", "var3", "var4","var5","var6"), varLabel = c("a label", NA, "another label", NA, NA, NA), stringsAsFactors = FALSE),
                valLabels = data.frame(varName = c("var1", "var1", "var2", "var2", "var2","var3", "var3","var5","var6"), value = c(-96, -99, -99, 0, 1,-96, -99, -9,-9),
                                       valLabel = c("miss1", "miss2", "miss2","right","wrong","miss1", "miss2", NA, NA),
                                       missings = c("miss", "miss", "miss","valid","valid","miss", "miss", "miss", "miss"), stringsAsFactors = FALSE))


f_txt <- tempfile(fileext = ".txt")
f_sps <- gsub("txt$", "sps", f_txt)
write_spss2(g, txtPath = f_txt, dec=",")

out <- read.csv2(f_txt, header=FALSE)
syntax <- readChar(f_sps, file.info(f_sps)$size)

write_spss2(g, txtPath = f_txt, dec=",", changeFormat=FALSE)

out2 <- read.csv2(f_txt, header=FALSE)
syntax2 <- readChar(f_sps, file.info(f_sps)$size)

test_that("Write spss 2 overall", {

  expect_equal(out, data.frame(V1 = c("ab c","bb"), V2 = c(1,NaN), V3 = c(1.01, 2.00), V4 = c(1.0001, 4.00187243564195786431587643596),V5 = c("ab c","b'b"),V6 = c("ab c","b'b"),stringsAsFactors = FALSE))
  expect_true(grepl("PRESERVE.\r\n SET DECIMAL", syntax))
  expect_true(grepl("GET DATA  /TYPE=TXT\r\n", syntax))
  expect_true(grepl("/DELCASE=LINE\r\n  /DELIMITERS=\";\"\r\n  /QUALIFIER='\"'\r\n  /ARRANGEMENT=DELIMITED\r\n  /FIRSTCASE=1\r\n  /DATATYPEMIN PERCENTAGE=95.0\r\n  /VARIABLES=\r\n", syntax))
  expect_true(grepl("EXECUTE.", syntax))
})

r1 <- list()
r1$labels <- g$labels
stopifnot(identical(unique(r1$labels$varName),names(g$dat)))
r1$varInfo <- unique(r1$labels[, c("varName", "varLabel", "format")])
r1$valInfo <- unique(r1$labels[which(!is.na(r1$labels$value)), c("varName", "value", "valLabel", "missings")])
r1$misInfo <- unique(r1$labels[which(!is.na(r1$labels$value) & r1$labels$missings == "miss"), c("varName", "value", "valLabel", "missings")])
r1$chv <- sapply(g$dat, is.character)

test_that("Format and writeHeader", {
  expect_true(grepl("var1 A4\r\nvar2 F2\r\nvar3 F4.2\r\nvar4 F16.14\r\nvar5 A4\r\nvar6 A4\r\n", syntax))
  expect_true(grepl("var1 NA\r\nvar2 NA\r\nvar3 NA\r\nvar4 NA\r\nvar5 NA\r\nvar6 NA\r\n", syntax2))
  # expect_warning(x1 <- writeHeader(r1, f_txt, f_sps, dec=",", fileEncoding="UTF-8"), "Format statement still contains 'NA' values, SPSS syntax will probably not work. Consider changeFormat=TRUE.")
})



#
#
#
# test_that("checkMissings2", {
#   expect_message(labs <- checkMissings2(g$labels, changeMeta=TRUE, verbose=TRUE), "Declaration will be changed, because changeMeta=TRUE.")
#   expect_message(labs2 <- checkMissings2(g$labels, changeMeta=FALSE, verbose=TRUE), "Info: Some missings are labelled without the keyword 'missing' in their label.")
#   expect_equal(labs$missings, rep("valid",8))
#   expect_equal(labs2$missings, g$labels$missings)
#   labsx <- g$labels
#   labsx$missings[labsx$valLabel=="miss1"] <- "valid"
#   labsx$valLabel[labsx$valLabel=="miss1"] <- "missing"
#   expect_message(checkMissings2(labsx, changeMeta=TRUE, verbose=TRUE), "Declaration will be changed, because changeMeta=TRUE.")
#   expect_message(checkMissings2(labsx, changeMeta=FALSE, verbose=TRUE), "Info: Some values are labelled 'missing' but are not declared as missing.")
# })
#
#
# test_that("writeData", {
#   g1<-writeData(g, filePath = f_txt, dec=",", fileEncoding = "UTF-8")
#
#   expect_true(identical(g1$dat, data.frame(var1= c("ab c", "bb"), var2=c(1, NaN), var3=c(1.01,2.00), var4=c(1.0001, 4.00187243564195786431587643596), xxxtgw=c(1,1))))
#   expect_true(setequal(g1$labels[9,],  data.frame(varName=c("xxxtgw","a"),varLabel=c(NA,"a"),format=c(NA,"a"),display_width=c(NaN,1),labeled=c("no","no"),value=c(NaN,1),valLabel=c(NA,"a"),missings=c(NA,"a"),stringsAsFactors = FALSE)[1,]))
#
# })
#
#
#
# test_that("createInputWriteFunctions", {
#    r1<-createInputWriteFunctions(g)
#
#   expect_true(identical(r1$labels$varName, c("var1", "var1", "var2", "var2", "var2", "var3", "var3", "var4")))
#   expect_true(identical(r1$labels$format, as.character(c("A4","A4",rep("F4",6)))))
#   expect_true(identical(r1$varInfo$varName, c("var1", "var2", "var3", "var4")))
#   expect_true(identical(r1$valInfo$varName, c("var1", "var1", "var2", "var2", "var2", "var3", "var3")))
#   expect_true(identical(r1$misInfo$varName, c("var1", "var1", "var2", "var3", "var3")))
#   expect_true(identical(as.numeric(r1$lengths), c(4,2,2,1)))
#   expect_true(identical(r1$dl.varnames, c("var1 (A4)", "var2 (F2)", "var3 (F4.2)", "var4 (F16.14)")))
#   expect_true(identical(unname(r1$chv), c(TRUE, FALSE, FALSE, FALSE)))
# })
#
#
#

#
# test_that("writeVaLab", {
#   expect_true(grepl("VARIABLE LABELS", syntax))
#   expect_true(grepl("VALUE LABELS", syntax))
#   expect_true(grepl("var3 \"another label\"", syntax))
#   expect_true(grepl("var1", syntax))
#   expect_true(grepl("-99 \"miss2\"", syntax))
#   expect_true(grepl("1 \"wrong\"", syntax))
# })
#
#
# test_that("writeMisCode", {
#   expect_true(grepl("MISSING VALUES", syntax2))
#   expect_true(grepl("var1 \\('-99','-96'\\)", syntax2))
#   expect_true(grepl("var2 \\(-99\\)", syntax2))
#   expect_true(grepl("var3 \\(-99,-96\\)", syntax2))
#   r1<-createInputWriteFunctions(g)
#   r1$misInfo[3:5,1] <- "var1"
#   expect_message(writeMisCode(r1,syntaxPath = f_sps), "Too many missing values for character variable 'var1'. SPSS allows only three missing values for character variables. I will take the first 3.")
#   r1$misInfo[1:5,1] <- "var3"
#   writeMisCode(r1,syntaxPath = f_sps)
#   syntax3 <- readChar(f_sps, file.info(f_sps)$size)
#   expect_true(grepl("-99 THRU -96", syntax3))
# })
#
