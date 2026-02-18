# Tests for retry logic and cache management -----------------------------------

# ---------------------------------------------------------------------------
# Retry / backoff (via .oir_get_raw)
# ---------------------------------------------------------------------------

test_that(".oir_get_raw throws oireachtas_api_error for 404", {
  # Stub httr::GET to return a 404
  local_mocked_bindings(
    GET = function(...) {
      structure(
        list(status_code = 404L,
             headers     = list(),
             content     = chartr("", "", raw(0))),
        class = "response"
      )
    },
    .package = "httr"
  )
  expect_error(
    oiReachtas:::.oir_get_raw("/nonexistent"),
    class = "oireachtas_api_error"
  )
})

test_that(".oir_get_raw retries on 429 and succeeds on second attempt", {
  call_count <- 0L
  local_mocked_bindings(
    GET = function(...) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        # First call: 429
        structure(
          list(status_code = 429L,
               headers     = list(`retry-after` = "0"),
               content     = chartr("", "", raw(0))),
          class = "response"
        )
      } else {
        # Second call: 200 with minimal JSON body
        body <- charToRaw('{"results":{"items":[]}}')
        structure(
          list(status_code = 200L,
               headers     = list(`content-type` = "application/json"),
               content     = body,
               url         = "https://api.oireachtas.ie/v1/test"),
          class = "response"
        )
      }
    },
    .package = "httr"
  )

  withr::with_options(
    list(oiReachtas.max_retries = 2L, oiReachtas.retry_wait = 0),
    {
      result <- oiReachtas:::.oir_get_raw("/test")
      expect_equal(call_count, 2L)
      expect_type(result, "list")
    }
  )
})

test_that(".oir_get_raw fails after exhausting retries on repeated 429", {
  local_mocked_bindings(
    GET = function(...) {
      structure(
        list(status_code = 429L,
             headers     = list(`retry-after` = "0"),
             content     = chartr("", "", raw(0))),
        class = "response"
      )
    },
    .package = "httr"
  )
  withr::with_options(
    list(oiReachtas.max_retries = 2L, oiReachtas.retry_wait = 0),
    expect_error(
      oiReachtas:::.oir_get_raw("/test"),
      class = "oireachtas_api_error"
    )
  )
})

# ---------------------------------------------------------------------------
# Cache management
# ---------------------------------------------------------------------------

test_that("oir_cache_clear runs without error", {
  expect_message(oir_cache_clear(), regexp = "Cache cleared")
})

test_that("oir_cache_info returns a list with n_entries and keys", {
  info <- oir_cache_info()
  expect_type(info, "list")
  expect_named(info, c("n_entries", "keys"))
  expect_type(info$n_entries, "integer")
  expect_type(info$keys, "character")
})
