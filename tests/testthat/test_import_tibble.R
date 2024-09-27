

test_that("Error",{
  expect_error(import_tibble(cars),
               "tibble must be a tibble.")
})


test_that("import_tibble works the same as import_spss",{
  import_spss_input <- import_spss(test_path("helper_spss.sav"))
  haven_input <- haven::read_sav(test_path("helper_spss.sav"))
  out <- import_tibble(haven_input)

  expect_equal(out, import_spss_input)
})




