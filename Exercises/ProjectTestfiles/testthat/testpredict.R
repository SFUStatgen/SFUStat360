library(mars)
load("testpredict.RData")
test_that("predict.mars() returns the correct predictions on the same data used to fit the model", {
  expect_equal(predictmars(testmars), testpredict)} )
test_that("predict.mars() returns the correct predictions on new data", {
  expect_equal(predictmars(testmars,newdata=marstestdata), testpredict)} )
