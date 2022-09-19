
# load test data (df1, df2, pkList, fkList)
# load(file = "tests/testthat/helper_data.rda")
load(file = "helper_data.rda")

### check missings
df1b <- df1
df1b$dat <- rbind(df1$dat, df1$dat)

df5 <- df4 <- df3 <- df1b
df3$dat[, "V1"] <- c(4, 1, 3, 1)

test_that("Errors", {
  expect_error(checkUniqueness(df1, varName = "V1", idVar = "ID1"),
               "'idVar' is unique per row in 'GADSdat' and checking for uniqueness is obsolete.")
})

test_that("No flagging", {
  out <- checkUniqueness(df1b, varName = "V1", idVar = "ID1")
  expect_true(out)
})

test_that("Correct flagging and output", {
  out <- checkUniqueness(df3, varName = "V1", idVar = "ID1")
  comp <- df3$dat[c(1, 3), ]
  row.names(comp) <- NULL
  expect_equal(out, comp)
})

test_that("Correct flagging and output for data.frames", {
  out <- checkUniqueness(df3$dat, varName = "V1", idVar = "ID1")
  comp <- df3$dat[c(1, 3), ]
  row.names(comp) <- NULL
  expect_equal(out, comp)
})



l <- 100000
long_df <- data.table::data.table(id = sort(rep(1:l, 15)),
                         v1 = sort(rep(1:l, 15)),
                         imp = rep(1:15, l))
#checkUniqueness2(as.data.frame(long_df), varName = "v1", idVar = "id", impVar = "imp")
#checkUniqueness3(as.data.frame(long_df), varName = "v1", idVar = "id", impVar = "imp")

l <- 100000
long_df_false <- data.table::data.table(id = sort(rep(1:l, 15)),
                                  v1 = sort(rep(1:l, 15)),
                                  imp = rep(1:15, l))
long_df_false[nrow(long_df_false), "v1"] <- 999

l <- 100000
long_df_err <- data.table::data.table(id = sort(rep(1:l, 1)),
                                  v1 = sort(rep(1:l, 1)),
                                  imp = rep(1, l))

long_df_uneven <- long_df[1:200, ]

test_that("fast Version: errors", {
  expect_message(out <- checkUniqueness2(long_df_err, varName = "v1", idVar = "id", impVar = "imp"),
               "'idVar' is unique per row in 'GADSdat' and checking for uniqueness is obsolete.")
  expect_true(out)
})

test_that("fast Version: fast enough", {
  out <- checkUniqueness2(long_df, varName = "v1", idVar = "id", impVar = "imp")
  expect_true(out)
})

test_that("fast Version: Correct flagging and output", {
  long_df2 <- long_df
  long_df2[1, "v1"] <- 100
  out <- checkUniqueness2(long_df2, varName = "v1", idVar = "id", impVar = "imp")
  expect_false(out)

  out <- checkUniqueness2(long_df_false, varName = "v1", idVar = "id", impVar = "imp")
  expect_false(out)
})

test_that("fast Version: different input length", {
  out <- checkUniqueness2(long_df_uneven, varName = "v1", idVar = "id", impVar = "imp")
  expect_true(out)
  ## what should the output be? -> call SW; why is there length output = 2 in Julias example
})


test_that("fast Version: GADSdat", {
  long_gads <- import_DF(long_df[1:150, ])
  out <- checkUniqueness2(long_gads, varName = "v1", idVar = "id", impVar = "imp")
  expect_true(out)
})
