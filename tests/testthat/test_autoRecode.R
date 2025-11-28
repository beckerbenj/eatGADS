# load test data (df1, df2, pkList, fkList)
load(file = test_path("helper_data.rda"))
dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))

df <- data.frame(id = c(110, 115, 112, 110), var1 = c(1, 1, 3, 1))
g <- import_DF(df)

test_that("auto recode a variable", {
  out <- autoRecode(g, var = "id", var_suffix = "_new", label_suffix = "(recoded)")
  expect_equal(namesGADS(out), c("id", "var1", "id_new"))
  expect_equal(out$dat$id_new, c(1, 3, 2, 1))
  expect_equal(out$labels[3, 2], "(recoded)", fixed = TRUE)
})

test_that("auto recode a variable while overwriting it", {
  out <- autoRecode(g, var = "id", var_suffix = "", label_suffix = "(recoded)")
  expect_equal(namesGADS(out), c("id", "var1"))
  expect_equal(out$dat$id, c(1, 3, 2, 1))
  expect_equal(out$labels[1, 2], "(recoded)", fixed = TRUE)
})


test_that("save lookup", {
  f <- tempfile(fileext = ".csv")
  out <- autoRecode(g, var = "id", var_suffix = "_new", csv_path = f)
  expect_equal(namesGADS(out), c("id", "var1", "id_new"))
  expect_equal(out$dat$id_new, c(1, 3, 2, 1))

  lookup <- read.csv(f)
  expect_equal(names(lookup), c("oldValue", "newValue"))
  expect_equal(lookup$oldValue, c(110, 112, 115))
  expect_equal(lookup$newValue, c(1, 2, 3))
})


test_that("existing lookup", {
  existing_lookup <- data.frame(oldValue = c(110, 111, 112, 113), newValue = 1:4)

  f <- tempfile(fileext = ".csv")
  expect_warning(out <- autoRecode(g, var = "id", var_suffix = "_new", label_suffix = "Test",
                                   template = existing_lookup, csv_path = f),
                 "For variable id_new the following values are in the lookup table but not in the data: 111, 113")

  expect_equal(namesGADS(out), c("id", "var1", "id_new"))
  expect_equal(out$dat$id_new, c(1, 5, 3, 1))
  expect_equal(out$labels[3, 2], "Test", fixed = TRUE)

  lookup <- read.csv(f)
  expect_equal(names(lookup), c("oldValue", "newValue"))
  expect_equal(lookup$oldValue, c(110, 111, 112, 113, 115))
  expect_equal(lookup$newValue, 1:5)
})

test_that("existing lookup, no new cases", {
  existing_lookup <- data.frame(oldValue = c(110:115), newValue = 1:6)

  f <- tempfile(fileext = ".csv")
  expect_warning(out <- autoRecode(g, var = "id", var_suffix = "_new", template = existing_lookup, csv_path = f),
                 "For variable id_new the following values are in the lookup table but not in the data: 111, 113, 114")

  expect_equal(namesGADS(out), c("id", "var1", "id_new"))
  expect_equal(out$dat$id_new, c(1, 6, 3, 1))

  lookup <- read.csv(f)
  expect_equal(names(lookup), c("oldValue", "newValue"))
  expect_equal(lookup$oldValue, 110:115)
  expect_equal(lookup$newValue, 1:6)
})

test_that("existing lookup, duplicate new cases", {
  existing_lookup <- data.frame(oldValue = c(112, 115), newValue = c(3, 6))

  f <- tempfile(fileext = ".csv")
  out <- autoRecode(g, var = "id", var_suffix = "_new", template = existing_lookup, csv_path = f)
  expect_equal(out$dat$id_new, c(7, 6, 3, 7))

  lookup <- read.csv(f)
  expect_equal(lookup$oldValue, c(112, 115, 110))
  expect_equal(lookup$newValue, c(3, 6, 7))
})

test_that("Preserve NAs while applying a template", {
  expect_df1 <- df1 <- data.frame(idteach = 11001:11010,
                                  idclass = c(1001, NA, NA, 1002, NA, 1003:1005, NA, 1006))
  gads1 <- import_DF(df1)
  expect_df1$idclass <- c(1, NA, NA, 2, NA, 3:5, NA, 6)
  expect_gads1 <- import_DF(expect_df1)
  expect_template_intermediate <- data.frame(oldValue = 1001:1006,
                                             newValue = 1:6)

  expect_df2 <- df2 <- data.frame(idteach = 21001:21010,
                                  idclass = c(NA, 1001, NA, 1005, NA, NA, 1007:1008, NA, 1009))
  gads2 <- import_DF(df2)
  template_path <- tempfile(fileext = ".csv")
  expect_df2$idclass <- c(NA, 1, NA, 5, NA, NA, 7:8, NA, 9)
  expect_gads2 <- import_DF(expect_df2)
  expect_template_final <- data.frame(oldValue = 1001:1009,
                                      newValue = 1:9)

  gads1_reco <- autoRecode(GADSdat = gads1,
                           var = "idclass",
                           var_suffix = "",
                           label_suffix = "",
                           csv_path = template_path)
  template_intermediate <- read.csv(template_path)
  expect_equal(gads1_reco$dat, expect_gads1$dat)
  expect_equal(template_intermediate, expect_template_intermediate)

  gads2_reco <- suppressWarnings(autoRecode(GADSdat = gads2,
                                            var = "idclass",
                                            var_suffix = "",
                                            label_suffix = "",
                                            template = template_intermediate,
                                            csv_path = template_path))
  template_final <- read.csv(template_path)
  expect_equal(gads2_reco$dat, expect_gads2$dat)
  expect_equal(template_final, expect_template_final)
})
