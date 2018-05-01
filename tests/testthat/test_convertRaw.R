library(skynet)

context("Test Convert Raw")

test_that("Convert Raw", {
  temp <- tempdir()
  convertRaw(skynet_example("Origin_and_Destination_Survey_DB1BCoupon_2001_1.csv"),
             skynet_example("Origin_and_Destination_Survey_DB1BTicket_2001_1.csv"),
             path = temp)
  expect_true(file.exists(tempdir()))
})
