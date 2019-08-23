context("getPC1")

data(aclust.listDemo)
data(ExampleMvalue_train)
data(ExampleMvalue_test)

test <- getPC1(
  clust_ls = aclust.listDemo,
  train_df = ExampleMvalue_train[,-1],
  test_df = ExampleMvalue_test[,-1]
)

test_that("getPC1() returns an S3 list", {
  expect_type(test, "list")
})

test_that("getPC1() has 3 elements", {
  expect_equal(length(test), 3)
})
