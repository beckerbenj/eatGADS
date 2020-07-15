
### Name transformation
# rawDat_names <- haven::read_spss("c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_spss_names.sav", user_na = TRUE)
rawDat_names <- haven::read_spss("helper_spss_names.sav", user_na = TRUE)

test_that("Variable name are transformed correctly ", {
  expect_identical(transf_names(names(rawDat_names)[1]), "groupVar")
  expect_identical(transf_names(names(rawDat_names)[2]), "var_1")
  expect_identical(transf_names("Select"), "SelectVar")
})

test_that("Variable name transformation is reported correctly ", {
 expect_message(transf_names(names(rawDat_names)[1]), "group has been renamed to groupVar")
 expect_message(transf_names(names(rawDat_names)[2]), "var.1 has been renamed to var_1")
})







