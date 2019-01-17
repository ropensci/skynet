library(skynet)

context("Test Download DB1B")

test_that("Download DB1B", {
  download_db1b(2011, 1)
  expect_output(str(nrow(OD_2011Q1)), "8592669")
  expect_length(OD_2011Q1, 19)
})

