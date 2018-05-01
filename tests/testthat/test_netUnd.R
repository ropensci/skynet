library(skynet)

context("Create Directed Network")



test_that("netUnd works", {
  # Run simple net test
  test <- make.netUnd(OD_Sample)
  expect_output(str(test), "List of 3")
  expect_output(str(nrow(test$netUnd)), "1531")
})

test_that("make.netUnd with disp", {
  # Run simple net test
  test <- make.netUnd(OD_Sample, disp = TRUE, alpha = 0.003)
  expect_output(str(nrow(test$netUnd_disp)), "21")
  expect_length(test$gUnd_disp, 10)
  expect_length(test$nodes, 13)
  expect_output(str(class(test$netUnd_disp)), "data.frame")
  expect_output(str(class(test$nodes)), "data.frame")
  expect_output(str(class(test$gUnd_disp)), "igraph")
})

test_that("make.netUnd with cap", {
  # Run simple net test
  test <- make.netUnd(OD_Sample, cap = TRUE, pct = 10)
  expect_output(str(nrow(test$netUnd_cap)), "126")
  expect_length(test$gUnd_cap, 10)
  expect_length(test$nodes, 13)
  expect_output(str(class(test$netUnd_cap)), "data.frame")
  expect_output(str(class(test$nodes)), "data.frame")
  expect_output(str(class(test$gUnd_cap)), "igraph")
})


test_that("make.netUnd with metro", {
  # Run simple net test
  test <- make.netUnd(OD_Sample, metro = TRUE)
  expect_output(str(nrow(test$netUnd)), "1211")
  expect_length(test$gUnd, 10)
  expect_length(test$nodes, 8)
  expect_output(str(class(test$netUnd)), "data.frame")
  expect_output(str(class(test$nodes)), "data.frame")
  expect_output(str(class(test$gUnd)), "igraph")
})
