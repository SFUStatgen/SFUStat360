library(mars)
load("testmars.RData")
test_that("mars() returns the correct object", {
  expect_equal(mars(y~.,data=marstestdata,control=testmc), 
               testmars, ignore_attr=TRUE)
})

