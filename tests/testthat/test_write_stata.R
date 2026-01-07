

### load test data
# example_df <- getGADS(filePath = "tests/testthat/helper_database.db")
example_df <- getGADS(filePath = "helper_dataBase.db")
# label_df <- labelsGADS(filePath = "tests/testthat/helper_database.db")
label_df <- labelsGADS(filePath = "helper_dataBase.db")

label_df_V2 <- label_df[which(label_df == "V2"), ]

expected_ID1 <- list(format.spss = "F8.2")
expected_V2 <- list(label = "Variable 2",
                    format.spss = "F10.2",
                    na_values = 99,
                    class = c("haven_labelled_spss", "haven_labelled"),
                    labels = c(mis = 99))


### write SPSS
test_that("GADSdat correctly written to dta", {
  # write_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_write_spss.sav")
  sav_path <- tempfile(fileext = ".dta")
  expect_warning(write_stata(example_df, filePath = sav_path),
                 "Missing codes and variable formats are dropped when writing to '.dta'.")

  #test_df <- export_tibble(example_df)
  #test_df2 <- haven::read_sav("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/other_code/helper_write_spss_manual.sav", user_na = TRUE)
  #str(test_df)
  #str(test_df2)

  df2 <- import_stata(sav_path)
  # df2 <- import_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_write_spss.sav")
  df3 <- example_df
  df3$labels$format <- NA_character_
  df3$labels$missings[3] <- "valid"
  expect_equal(df2$dat, df3$dat)
  rownames(df3$labels) <- NULL
  expect_equal(df2$labels, df3$labels)
})


test_that("Write strings longer than 255", {
  #g <- import_spss("tests/testthat/helper_longstring.sav")
  suppressWarnings(g <- import_spss("helper_longstring.sav"))
  f <- tempfile(fileext = ".dta")
  suppressWarnings(write_stata(g, filePath = f))
  out <- haven::read_stata(f)
  expect_equal(dim(out), c(1, 2))
})


test_that("Abort with error for a hard-incompatible dataset", {
  temppath <- tempfile(fileext = ".dta")
  bad_gads <- example_df
  bad_gads$labels$value[bad_gads$labels$value == 99] <- 99.9
  expect_error(write_stata(bad_gads, temppath))
})

test_that("Warn about a soft-incompatible dataset", {
  temppath <- tempfile(fileext = ".dta")
  expect_warning(write_stata(pisa, temppath))
})
