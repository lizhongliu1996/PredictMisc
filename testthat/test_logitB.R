context("logitB")

test_that("logitB() returns an S3 list", {
  expect_equal(logitB(0.6, B = 2), 0.5849625)
})
