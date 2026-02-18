test_that("get_questions validates question_type", {
  expect_error(
    get_questions(question_type = "informal"),
    class = "oireachtas_invalid_param"
  )
})

test_that("get_questions validates date parameters", {
  expect_error(
    get_questions(date_start = "2023/01/01"),
    class = "oireachtas_invalid_date"
  )
})

test_that(".parse_questions returns empty tibble for empty input", {
  result <- .parse_questions(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})
