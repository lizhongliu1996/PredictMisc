context("caretWrapper")

data(ExampleMvalue_train)

rf_Fit <- caretWrapper(
  trainTable_df = ExampleMvalue_train,
  respCol_index = 1,
  predMethod = "rf",
  seed_int = 120,
  whichCVfold_int = 5,
  ncores = 2)

svm_Fit <- caretWrapper(
  trainTable_df = ExampleMvalue_train,
  respCol_index = 1,
  predMethod = "svm",
  seed_int = 120,
  whichCVfold_int = 5,
  ncores = 2)

test_that("caretWrapper() returns an S3 list", {
  expect_type(rf_Fit, "list")
  expect_type(svm_Fit, "list")
})

test_that("caretWrapper() has 20 elements", {
  expect_equal(length(rf_Fit), 20)
  expect_equal(length(svm_Fit), 20)
})
