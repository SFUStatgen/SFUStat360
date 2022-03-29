library(mars)
load("testbwd_stepwise.RData")
test_that("bwd_stepwise() returns the correct object", {
  expect_equal(bwd_stepwise(testfwd,testmc), testbwd)
})

