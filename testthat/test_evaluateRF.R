context("evaluateRF")

data(ExampleMvalue_test)
data(rf_Fit)

performance <- evaluateRF(
  caret_fit = rf_Fit,
  testTable = ExampleMvalue_test,
  respCol_index = 1,
  whichRep_int = 1,
  whichCVfold_int = 1
)

test_that("evaluateRF() returns an S3 data frame", {
  expect_s3_class(performance, "data.frame")
})

test_that("evaluateRF() has 14 elements", {
  expect_equal(ncol(performance), 14)
})
