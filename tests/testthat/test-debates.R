test_that("get_debates validates date parameters", {
  expect_error(
    get_debates(date_start = "01-01-2023"),
    class = "oireachtas_invalid_date"
  )
})

test_that(".parse_debates returns empty tibble for empty input", {
  result <- .parse_debates(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})
