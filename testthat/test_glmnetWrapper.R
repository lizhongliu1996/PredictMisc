context("glmnetWrapper")

data(ExampleMvalue_train)

fit <- glmnetWrapper(alpha = 1,
                     trainTable_df = ExampleMvalue_train,
                     respCol_index = 1,
                     family = "binomial",
                     seed_int = 120,
                     whichCVfold_int = 5,
                     whichRep_int = 1)

test_that("glmnetWrapper() returns an S3 list", {
  expect_s3_class(fit, "cv.glmnet")
})

test_that("Structure of object returned by glmnetWrapper", {
  expect_equal(length(fit), 10)
  expect_equal(length(fit$glmnet.fit), 13)
})
