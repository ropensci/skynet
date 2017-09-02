library(skynet)

context("Create Directed Network")

test <- make.netDir(OD_Sample)

test_that("netDir works", {
  expect_output(str(test), "List of 3")
})
