library(skynet)

context("Create Path Network")



test_that("make_net_path works", {
  # Run simple net test
  test <- make_net_path(OD_Sample)
  expect_length(test, 14)
  expect_output(str(nrow(test)), "2182")
})

test_that("make_net_path with leg", {
  # Run simple net test
  test <- make_net_path(OD_Sample, leg = TRUE)
  expect_length(test$netLegCount$legCount, 2749)
  expect_length(test$netLegCount, 7)
})

test_that("make_net_path with zero tickets", {
  # Run simple net test
  test <- make_net_path(OD_Sample, zero = TRUE)
  expect_length(test$pct_zero, 2182)
})

test_that("make_net_path with carriers", {
  # Run simple net test
  test <- make_net_path(OD_Sample, carrier = TRUE)
  expect_length(test$op_carrier, 2749)
})


test_that("make_net_path counts multi-leg passengers once", {
  # Two-leg itinerary BOS-ATL-LAX with 2 passengers
  x <- data.frame(mkt_id = c(1, 1), seq_num = 1:2, origin = c("BOS", "ATL"),
                  dest = c("ATL", "LAX"), itin_fare = 300, passengers = 2,
                  roundtrip = 0, itin_yield = 0.1, distance = c(900, 1900),
                  year = 2001, quarter = 1, op_carrier = "DL")
  test <- make_net_path(x)
  expect_equal(nrow(test), 1)
  expect_equal(test$passengers, 2)
  test <- make_net_path(x, zero = TRUE, carrier = TRUE)
  expect_equal(test$passengers, 2)
})
