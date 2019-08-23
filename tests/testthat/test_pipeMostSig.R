context("pipeMostSig")
data(Example_df)
data(pfcInfo_df)

test <- pipeMostSig(
  rowNum = 10,
  Beta_df = Example_df,
  beta2M = TRUE,
  respCol_index = 1,
  designInfo_df = pfcInfo_df,
  alphaValue = seq(0, 1, by = 0.1),
  ncores = 2,
  npredictors = 5000,
  predictMethod = "glmnet",
  outcome_type = "binomial",
  save = FALSE,
  resultPath = NULL
)

test2 <- pipeMostSig(
  rowNum = 10,
  Beta_df = Example_df,
  beta2M = FALSE,
  respCol_index = 1,
  designInfo_df = pfcInfo_df,
  alphaValue = seq(0, 1, by = 0.1),
  ncores = 2,
  npredictors = 5000,
  predictMethod = "glmnet",
  outcome_type = "binomial",
  save = FALSE,
  resultPath = NULL
)

test3 <- pipeMostSig(
  rowNum = 10,
  Beta_df = Example_df,
  beta2M = TRUE,
  respCol_index = 1,
  designInfo_df = pfcInfo_df,
  alphaValue = seq(0, 1, by = 0.1),
  ncores = 2,
  npredictors = 5000,
  predictMethod = "randomForest",
  outcome_type = "binomial",
  save = FALSE,
  resultPath = NULL
)

test4 <- pipeMostSig(
  rowNum = 10,
  Beta_df = Example_df,
  beta2M = FALSE,
  respCol_index = 1,
  designInfo_df = pfcInfo_df,
  alphaValue = seq(0, 1, by = 0.1),
  ncores = 2,
  npredictors = 5000,
  predictMethod = "randomForest",
  outcome_type = "binomial",
  save = FALSE,
  resultPath = NULL
)

test5 <- pipeMostSig(
  rowNum = 10,
  Beta_df = Example_df,
  beta2M = FALSE,
  respCol_index = 1,
  designInfo_df = pfcInfo_df,
  alphaValue = seq(0, 1, by = 0.1),
  ncores = 2,
  npredictors = 5000,
  predictMethod = "svm",
  outcome_type = "binomial",
  save = FALSE,
  resultPath = NULL
)

test6 <- pipeMostSig(
  rowNum = 10,
  Beta_df = Example_df,
  beta2M = FALSE,
  respCol_index = 1,
  designInfo_df = pfcInfo_df,
  alphaValue = seq(0, 1, by = 0.1),
  ncores = 2,
  npredictors = 5000,
  predictMethod = "svm",
  outcome_type = "binomial",
  save = FALSE,
  resultPath = NULL
)

test_that("pipeMostSig() returns an S3 list", {
  expect_type(test, "list")
  expect_type(test2, "list")
  expect_type(test3, "list")
  expect_type(test4, "list")
  expect_type(test5, "list")
  expect_type(test6, "list")
})

test_that("pipeMostSig() has 3 elements", {
  expect_equal(length(test), 3)
  expect_equal(length(test2), 3)
  expect_equal(length(test3), 3)
  expect_equal(length(test4), 3)
  expect_equal(length(test5), 3)
  expect_equal(length(test6), 3)
})
