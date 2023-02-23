

test_that("lower case", {
  y <- c("Hi", "HEllo", "greaT")
  out <- convertCase(y, case = "lower")
  expect_equal(out, c(c("hi", "hello", "great")))
})

test_that("upper case", {
  y <- c("Hi", "HEllo", "greaT")
  out <- convertCase(y, case = "upper")
  expect_equal(out, c(c("HI", "HELLO", "GREAT")))
})

test_that("first upper", {
  y <- c("Hi", "HEllo", "greaT", "just The First")
  out <- convertCase(y, case = "upperFirst")
  expect_equal(out, c(c("Hi", "Hello", "Great", "Just the first")))
})

test_that("data.frames", {
  out <- convertCase(mtcars)
  expect_equal(out, mtcars)
  out2 <- convertCase(iris)
  expect_equal(out2, iris)

  input <- data.frame(v1 = 1:3, v2 = c("Hi", "HEllo", "greaT"), v3 = 3:1, stringsAsFactors = FALSE)
  out3 <- convertCase(input)
  expect_equal(out3[[2]], c("hi", "hello", "great"))
  expect_equal(out3[[1]], input[[1]])
})


test_that("GADSdat", {
  input <- data.frame(v1 = 1:3, v2 = c("Hi", "HEllo", "greaT"), v3 = 3:1, stringsAsFactors = FALSE)
  input_g <- import_DF(input)

  expect_error(convertCase(input_g, vars = 1), "vars needs to be a character vector of at least length 1.")
  expect_error(convertCase(input_g, vars = namesGADS(input_g)), "v1 is not a character variable and can not be case converted.")

  out <- convertCase(input_g, vars = "v2")
  expect_equal(out$dat[[2]], c("hi", "hello", "great"))
  expect_equal(out$dat[[1]], input_g$dat[[1]])
})
