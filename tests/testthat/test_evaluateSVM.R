context("evaluateSVM")

data(ExampleMvalue_test)
data(svm_Fit)

performance <- evaluateSVM(
  caret_fit = svm_Fit,
  testTable = ExampleMvalue_test,
  respCol_index = 1,
  whichRep_int = 1,
  whichCVfold_int = 1
)

test_that("evaluateSVM() returns an S3 data frame", {
  expect_s3_class(performance, "data.frame")
})

test_that("evaluateSVM() has 14 elements", {
  expect_equal(ncol(performance), 14)
})
