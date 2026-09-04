facs_df <- data.frame(id = 1:3,
                      fac1 = c("Eng", "Aus", "Ger"),
                      fac2 = c("Ger", "Fra", "Ita"),
                      fac3 = c("Kor", "missing - omitted", "Alg"),
                      stringsAsFactors = TRUE)
facs_gads1 <- import_DF(facs_df)

facs_gads2 <- facs_gads1
facs_gads2 <- recodeGADS(facs_gads2, varName = "fac3",
                         oldValues = 3,
                         newValues = -99)
for(i in paste0("fac", 1:3)) {
  facs_gads2 <- changeMissings(facs_gads2, varName = i, value = -99, missings = "miss")
}

facs_gads3 <- changeMissings(facs_gads1, varName = "fac3",
                             value = 3, missings = "miss")

facs_gads4 <- recodeGADS(facs_gads3, varName = "fac3",
                         oldValues = 3, newValues = 7)

facs_gads5 <- changeValLabels(facs_gads1, varName = "fac1",
                              value = c(4:6, -97),
                              valLabel = c("Swe", "USA", "Den", "missing by design"))
facs_gads5 <- changeMissings(facs_gads5, varName = "fac1", value = -97, missings = "miss")

load(file = "helper_data.rda")


test_that("Invalid inputs are rejected", {
  bad_lookup <- data.frame(value = 1:3,
                           valllabel = letters[1:3],
                           missing = "miss")
  expect_error(assimilateValLabels(facs_gads1, varNames = namesGADS(facs_gads1),
                                   lookup = bad_lookup),
               regexp = "^'lookup'")

  expect_error(assimilateValLabels(expected_bigList),
               regexp = "^This method")
})

test_that("Assimilated valLabels are ordered alphabetically when no lookup is supplied", {
  # 2 variables
  out_2var <- assimilateValLabels(facs_gads1, varNames = paste0("fac", 1:2))
  meta_2var <- out_2var$labels[out_2var$labels$varName %in% c("fac1", "fac2"),
                               c("value", "valLabel")]
  rownames(meta_2var) <- NULL

  expected_meta2 <- data.frame(value = 1:5,
                              valLabel = sort(unique(c("Eng", "Aus", "Ger",
                                                       "Ger", "Fra", "Ita"))))
  expect_equal(out_2var$dat[, paste0("fac", 1:2)],
               data.frame(fac1 = match(facs_df$fac1, expected_meta2$valLabel),
                          fac2 = match(facs_df$fac2, expected_meta2$valLabel)))
  expect_equal(meta_2var, rbind(expected_meta2, expected_meta2))

  # 3 variables
  out_3var <- assimilateValLabels(facs_gads1, varNames = paste0("fac", 1:3))
  meta_3var <- out_3var$labels[out_3var$labels$varName %in% paste0("fac", 1:3),
                               c("value", "valLabel")]
  rownames(meta_3var) <- NULL

  expected_meta3 <- data.frame(value = 1:8,
                              valLabel = sort(unique(c("Eng", "Aus", "Ger",
                                                       "Ger", "Fra", "Ita",
                                                       "Kor", "missing - omitted", "Alg"))))
  expect_equal(out_3var$dat[, paste0("fac", 1:3)],
               data.frame(fac1 = match(facs_df$fac1, expected_meta3$valLabel),
                          fac2 = match(facs_df$fac2, expected_meta3$valLabel),
                          fac3 = match(facs_df$fac3, expected_meta3$valLabel)))
  expect_equal(meta_3var, rbind(expected_meta3, expected_meta3, expected_meta3))
})

test_that("Lookup overwrites label order but does not introduce holes to value range", {
  country_lookup <- data.frame(value = c(1:3),
                               valLabel = c("Ger", "Ita", "Pol"),
                               missings = "valid")
  out <- assimilateValLabels(facs_gads1, varNames = paste0("fac", 1:3), lookup = country_lookup)
  meta <- out$labels[out$labels$varName %in% paste0("fac", 1:3), c("value", "valLabel")]
  rownames(meta) <- NULL

  expected_meta <- data.frame(value = 1:8,
                              valLabel = c("Ger", "Ita",
                                           "Alg", "Aus", "Eng", "Fra", "Kor",
                                           "missing - omitted"))
  expect_equal(out$dat[, paste0("fac", 1:3)],
               data.frame(fac1 = match(facs_df$fac1, expected_meta$valLabel),
                          fac2 = match(facs_df$fac2, expected_meta$valLabel),
                          fac3 = match(facs_df$fac3, expected_meta$valLabel)))
  expect_equal(meta, rbind(expected_meta, expected_meta, expected_meta))
})

test_that("Lookup overwrites missing tags", {
  country_lookup <- data.frame(value = 95,
                               valLabel = "missing - omitted",
                               missings = "valid")
  expect_warning(out <- assimilateValLabels(facs_gads2, varNames = paste0("fac", 3:1),
                                            lookup = country_lookup),
                 regexp = "^Some values marked")
  meta <- out$labels[out$labels$varName %in% paste0("fac", 1:3), c("value", "valLabel")]
  rownames(meta) <- NULL

  expected_meta <- data.frame(value = c(1:7, 95),
                              valLabel = c(sort(c("Ger", "Ita",
                                                  "Alg", "Aus", "Eng", "Fra", "Kor")),
                                           "missing - omitted"))
  expect_equal(out$dat[, paste0("fac", 1:3)],
               data.frame(fac1 = match(c("Eng", "Aus", "Ger"), expected_meta$valLabel),
                          fac2 = match(c("Ger", "Fra", "Ita"), expected_meta$valLabel),
                          fac3 = c(7, 95, 1)))
  expect_equal(meta, rbind(expected_meta, expected_meta, expected_meta))
})

test_that("Missing tags of only the first variable are reused", {
  out1 <- assimilateValLabels(facs_gads2, varNames = paste0("fac", 1:3))
  missing_meta1 <- out1$labels[out1$labels$valLabel == "missing - omitted" &
                                 !is.na(out1$labels$valLabel), ]
  expect_identical(missing_meta1$value[[1]], 8)
  expect_identical(missing_meta1$missings[[1]], "valid")

  out2 <- assimilateValLabels(facs_gads2, varNames = paste0("fac", 3:1))
  missing_meta2 <- out2$labels[out2$labels$valLabel == "missing - omitted" &
                                 !is.na(out2$labels$valLabel), ]
  expect_identical(missing_meta2$value[[1]], -99)
  expect_identical(missing_meta2$missings[[1]], "miss")
})

test_that("Missing tags inside the valid value range cause a warning", {
  expect_warning(out <- assimilateValLabels(facs_gads3, varNames = paste0("fac", 3:1)),
                 regexp = "^Some missing")
  missing_meta <- out$labels[out$labels$valLabel == "missing - omitted" &
                               !is.na(out$labels$valLabel), ]
  expect_identical(missing_meta$value[[1]], 3)
  expect_identical(missing_meta$missings[[1]], "miss")
})

test_that("Valid values are arranged around a missing tag if necessary", {
  suppressWarnings(out3 <- assimilateValLabels(facs_gads3, varNames = paste0("fac", 3:1)))
  expect_warning(out4 <- assimilateValLabels(facs_gads4, varNames = paste0("fac", 3:1)),
                 regexp = "^Some missing")
  korea_meta3 <- out3$labels[out3$labels$valLabel == "Kor" & !is.na(out3$labels$valLabel), ]
  korea_meta4 <- out4$labels[out4$labels$valLabel == "Kor" & !is.na(out3$labels$valLabel), ]
  expect_identical(korea_meta3$value[[1]], 8)
  expect_identical(korea_meta4$value[[1]], 8)
})

test_that("Only existing labels are retained", {
  out1 <- assimilateValLabels(facs_gads1, varNames = paste0("fac", 1:3))
  out5 <- assimilateValLabels(facs_gads5, varNames = paste0("fac", 1:3))
  expect_identical(out1, out5)
})
