test_that(".oir_validate_date accepts valid dates", {
  expect_invisible(.oir_validate_date("2023-01-15", "test_date"))
  expect_invisible(.oir_validate_date(NULL, "test_date"))
})

test_that(".oir_validate_date rejects invalid formats", {
  expect_error(.oir_validate_date("15-01-2023", "test_date"), class = "oireachtas_invalid_date")
  expect_error(.oir_validate_date("2023/01/15", "test_date"), class = "oireachtas_invalid_date")
  expect_error(.oir_validate_date("not-a-date", "test_date"), class = "oireachtas_invalid_date")
})

test_that(".oir_pagination returns correct list", {
  p <- .oir_pagination(limit = 10L, skip = 20L)
  expect_equal(p$limit, 10L)
  expect_equal(p$skip, 20L)
})

test_that(".null_na returns NA for NULL", {
  expect_true(is.na(.null_na(NULL)))
})

test_that(".null_na returns value when present", {
  expect_equal(.null_na("hello"), "hello")
})
