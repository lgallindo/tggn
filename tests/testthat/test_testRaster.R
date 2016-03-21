library(tgnn)
context("Return type")

test_that("str_length is number of characters", {
  expect_true(is.matrix(testRaster()))
  expect_true(is.matrix(testRaster(length=100, size=1000, activity=0.25, RNGdata=NULL)))
})
