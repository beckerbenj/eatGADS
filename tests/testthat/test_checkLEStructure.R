
test_that("checkLEStructure", {
  # checkLEStructure(filePath1 = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_comp.db", filePath2 = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_comp.db", lePath = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_le.db")
  expect_silent(checkLEStructure(filePath1 = "helper_comp.db", filePath2 = "helper_comp.db", lePath = "helper_le.db"))
  le_mes <- capture_messages(out <- checkLEStructure(filePath1 = "helper_comp.db", filePath2 = "helper_comp.db", lePath = "helper_le_wrong.db"))
  expect_equal(le_mes[[1]], "The following variables have linking errors but are not variables in data base 1: other\n")
  expect_equal(le_mes[[2]], "The following variables have linking errors but are not variables in data base 2: other\n")
  expect_equal(le_mes[[3]], "The linking error data base contains variables other than linking errors and key variables.\n")
  expect_equal(le_mes[[4]], "The following variables are key variables in the Linking Error data base but are not variables in data base 1: test_pk\n")
  expect_equal(le_mes[[5]], "The following variables are key variables in the Linking Error data base but are not variables in data base 2: test_pk\n")
  expect_equal(out, list(dep_notIn_nam1 = "other", dep_notIn_nam2 = "other",
                         key_notIn_nam1 = "test_pk", key_notIn_nam2 = "test_pk"))
})
