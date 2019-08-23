context("predict_wrapper")

data(ExampleMvalue_test)
data(rf_Fit)

test1 <- predict_wrapper(
    predictMethod = "glmnet",
    alphaValue = seq(0, 1, by = 0.1),
    PhenoTrain_df = ExampleMvalue_train,
    PhenoTest_df = ExampleMvalue_test,
    respCol_index = 1,
    outcome_type = "binomial",
    seed_int = 123,
    whichRep_int = 1,
    whichCVfold_int = 1,
    ncores = 2
    )

test2 <- predict_wrapper(
  predictMethod = "randomForest",
  alphaValue = seq(0, 1, by = 0.1),
  PhenoTrain_df = ExampleMvalue_train,
  PhenoTest_df = ExampleMvalue_test,
  respCol_index = 1,
  outcome_type = "binomial",
  seed_int = 123,
  whichRep_int = 1,
  whichCVfold_int = 1,
  ncores = 2
)

test3 <- predict_wrapper(
  predictMethod = "svm",
  alphaValue = seq(0, 1, by = 0.1),
  PhenoTrain_df = ExampleMvalue_train,
  PhenoTest_df = ExampleMvalue_test,
  respCol_index = 1,
  outcome_type = "binomial",
  seed_int = 123,
  whichRep_int = 1,
  whichCVfold_int = 1,
  ncores = 2
)

test_that("predict_wrapper returns an list", {
  expect_type(test1, "list")
  expect_type(test2, "list")
  expect_type(test3, "list")
})

test_that("predict_wrapper has 2 elements", {
  expect_equal(length(test1), 2)
  expect_equal(length(test2), 2)
  expect_equal(length(test3), 2)
})
