test_that("get_divisions validates date parameters", {
  expect_error(
    get_divisions(date_start = "01-Jan-2023"),
    class = "oireachtas_invalid_date"
  )
})

test_that(".parse_divisions returns empty tibble for empty input", {
  result <- .parse_divisions(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})
