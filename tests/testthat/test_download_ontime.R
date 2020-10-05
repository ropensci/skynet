library(skynet)

context("Test Download Ontime")

test_that("Download Ontime", {
  skip_on_cran()
  download_ontime(2011, 1)
  expect_output(str(nrow(ontime_2011_1)), "494400")
  expect_length(ontime_2011_1, 26)
})
