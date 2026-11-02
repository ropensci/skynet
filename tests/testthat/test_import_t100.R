library(skynet)

context("Test Import T100")

test_that("Import T100", {
  import_t100(skynet_example("T100_2011_mkt.csv"))
  expect_output(str(nrow(T100_2011_mkt)), "85")
  expect_length(T100_2011_mkt, 13)
})

test_that("Import T100 including non-scheduled flights", {
  import_t100(skynet_example("T100_2011_mkt.csv"), nonsch = TRUE)
  expect_output(str(nrow(T100_2011_mkt)), "100")
  expect_length(T100_2011_mkt, 13)
})



test_that("Import T100 segment with auto = FALSE", {
  if(exists("T100_2011_seg", envir = globalenv())) rm("T100_2011_seg", envir = globalenv())
  test <- import_t100(skynet_example("T100_2011_int.csv"), auto = FALSE)
  expect_output(str(class(test)), "data.frame")
  expect_false(exists("T100_2011_seg", envir = globalenv()))
})
