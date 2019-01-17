library(skynet)

context("Test Import Ontime")

test_that("Import Ontime", {
  import_ontime(skynet_example("Ontime_2011_1.csv"))
  expect_output(str(nrow(ontime_2011_1)), "50")
  expect_length(ontime_2011_1, 26)
})



