library(skynet)

context("Test Download T100")

test_that("Download T100", {
  if(is.null(expect_message(download_t100(2011, "mkt")))){
    test_message <- paste("No connection 404")
    expect_equal(test_message, "No connection 404")
  }else{
  download_t100(2011, "seg")
  expect_output(str(nrow(T100_2011_seg)), "250828")
  expect_length(T100_2011_seg, 17)
  }
})

test_that("Download T100", {
  if(is.null(expect_message(download_t100(2011, "mkt")))){
    test_message <- paste("No connection 404")
    expect_equal(test_message, "No connection 404")
  }else{
  download_t100(2011, "mkt")
  expect_output(str(nrow(T100_2011_mkt)), "194371")
  expect_length(T100_2011_mkt, 13)
  }
})

