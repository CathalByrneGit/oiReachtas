test_that("get_members returns a tibble on a mocked response", {
  skip_if_not_installed("httptest")
  httptest::with_mock_api({
    result <- get_members(chamber = "dail", limit = 5L)
    expect_s3_class(result, "tbl_df")
  })
})

test_that("get_members validates date parameters", {
  expect_error(
    get_members(date_start = "01/01/2023"),
    class = "oireachtas_invalid_date"
  )
  expect_error(
    get_members(date_end = "2023-1-1"),
    class = "oireachtas_invalid_date"
  )
})

test_that(".parse_members returns empty tibble for empty input", {
  result <- .parse_members(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})
