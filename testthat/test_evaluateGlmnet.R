context("evaluateGlmnet")

data(ExampleMvalue_test)
data(glmnet_Fit_ls)

performance <- evaluateGlmnet(
  fit = glmnet_Fit_ls[[1]],
  alpha = seq(0, 1, by = 0.1)[1],
  testTable = ExampleMvalue_test,
  respCol_index = 1,
  whichRep_int = 1,
  whichCVfold_int = 1
)

test_that("evaluateGlmnet() returns an S3 data frame", {
  expect_s3_class(performance, "data.frame")
})

test_that("evaluateGlmnet() has 16 elements", {
  expect_equal(ncol(performance), 16)
})
