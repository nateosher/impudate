library(dplyr)


test_that("add_date_parts parses partial dates", {
  df <- tibble::tibble(date = c("2024", "2024-07", "2024-07-31", NA))
  out <- add_date_parts(df, date)

  expect_equal(out$date_year, c(2024L, 2024L, 2024L, NA_integer_))
  expect_equal(out$date_month, c(NA_integer_, 7L, 7L, NA_integer_))
  expect_equal(out$date_day, c(NA_integer_, NA_integer_, 31L, NA_integer_))
})

test_that("prefix overrides column name", {
  df <- tibble::tibble(date = c("2024-07-31"))
  out <- add_date_parts(df, date, prefix = "x")

  expect_true(all(c("x_year", "x_month", "x_day") %in% names(out)))
})

test_that("invalid handling modes behave as expected", {
  df <- tibble::tibble(date = c("2024-13-40", "2024-00-00"))

  out_allow <- add_date_parts(df, date, invalid = "allow")
  expect_equal(out_allow$date_month, c(13L, 0L))
  expect_equal(out_allow$date_day, c(40L, 0L))

  expect_warning(
    out_warn <- add_date_parts(df, date, invalid = "warn"),
    "Invalid month/day"
  )
  expect_equal(out_warn$date_month, c(NA_integer_, NA_integer_))
  expect_equal(out_warn$date_day, c(NA_integer_, NA_integer_))

  expect_error(
    add_date_parts(df, date, invalid = "error"),
    "Invalid month/day"
  )
})
