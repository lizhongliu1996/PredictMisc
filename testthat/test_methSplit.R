context("methSplit")

data(Example_df)
data(pfcInfo_df)

data_ls <- methSplit(
  rowNum = 1,
  Beta_df = Example_df,
  designInfo_df = pfcInfo_df
)

test_that("methSplit() returns an S3 list", {
  expect_type(data_ls, "list")
})

test_that("data_ls() has 2 elements", {
  expect_equal(length(data_ls), 2)
})
