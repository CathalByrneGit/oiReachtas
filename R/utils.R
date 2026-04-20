#' @keywords internal
.oireachtas_base_url  <- "https://api.oireachtas.ie/v1"

#' @keywords internal
.oireachtas_data_url  <- "https://data.oireachtas.ie"

# ---------------------------------------------------------------------------
# Raw HTTP layer (with retry / backoff)
# ---------------------------------------------------------------------------

#' Make a GET request to the Oireachtas API, with retry on transient errors
#'
#' Retries on HTTP 429 (rate limit) and 5xx (server error) responses using
#' exponential backoff.  The number of retries and the initial wait time are
#' controlled by `options("oiReachtas.max_retries")` (default 4) and
#' `options("oiReachtas.retry_wait")` (default 1 second).
#'
#' @param endpoint Character. API path (e.g. `"/members"`).
#' @param params Named list of query parameters.
#' @param ua Character. User-agent string.
#'
#' @return Parsed JSON as a list.
#' @keywords internal
.oir_get_raw <- function(endpoint,
                         params   = list(),
                         ua       = "oiReachtas R package") {
  url    <- paste0(.oireachtas_base_url, endpoint)
  params <- Filter(Negate(is.null), params)

  max_retries <- getOption("oiReachtas.max_retries", 4L)
  wait        <- getOption("oiReachtas.retry_wait",  1)   # seconds

  # Build a readable query string for logging
  qs <- paste(names(params), unlist(params), sep = "=", collapse = "&")
  message(sprintf("[oiReachtas] GET %s?%s", endpoint, qs))

  for (attempt in seq_len(max_retries + 1L)) {
    resp <- tryCatch(
      httr2::request(url)|>
        httr2::req_url_query(!!!params,.multi = "comma")|>
        httr2::req_error(is_error = \(r) FALSE)|>   # handle HTTP errors manually below
        httr2::req_perform(),
      error = function(e) {
        message(sprintf("[oiReachtas] Connection failed (attempt %d): %s",
                        attempt, conditionMessage(e)))
        if (attempt > max_retries) rlang::abort(
          paste0("Network error after ", max_retries, " retries: ", conditionMessage(e)),
          class = "oireachtas_network_error"
        )
        NULL
      }
    )

    # Network failure (resp is NULL) → back off and retry
    if (is.null(resp)) {
      .oir_backoff(attempt, wait)
      wait <- wait * 2
      next
    }

    status <- httr2::resp_status(resp)
    message(sprintf("[oiReachtas] HTTP %d %s", status, httr2::resp_status_desc(resp)))

    # Success
    if (status < 400L) {
      return(httr2::resp_body_json(resp))
    }

    # Log response body on error to show what the API said
    body_text <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) "(unreadable body)"
    )
    message(sprintf("[oiReachtas] Error body: %s",
                    substr(body_text, 1L, 500L)))

    # Retryable: 429 (rate limit) or transient 5xx
    retryable <- status %in% c(429L, 500L, 502L, 503L, 504L)
    if (retryable && attempt <= max_retries) {
      # resp_header() returns NULL when absent; as.numeric(NULL) = numeric(0), not NA
      retry_after <- suppressWarnings(
        as.numeric(httr2::resp_header(resp, "retry-after") %||% NA_real_)
      )
      sleep_for <- if (length(retry_after) == 1L && !is.na(retry_after) && retry_after > 0) {
        retry_after
      } else {
        wait
      }
      message(sprintf(
        "[oiReachtas] HTTP %d – retrying in %.0fs (attempt %d/%d)",
        status, sleep_for, attempt, max_retries
      ))
      Sys.sleep(sleep_for)
      wait <- wait * 2
      next
    }

    # Non-retryable or exhausted retries → throw
    .oir_check_response(resp)
  }
}

#' Exponential back-off helper
#' @keywords internal
.oir_backoff <- function(attempt, wait) {
  message(sprintf(
    "[oiReachtas] Network error – retrying in %.0fs (attempt %d)",
    wait, attempt
  ))
  Sys.sleep(wait)
}

# ---------------------------------------------------------------------------
# Memoised public-facing GET (initialised in .onLoad)
# ---------------------------------------------------------------------------

# Placeholder overwritten by .onLoad
#' @keywords internal
.oir_get <- NULL

# ---------------------------------------------------------------------------
# XML GET (also memoised)
# ---------------------------------------------------------------------------

#' Fetch an XML document from data.oireachtas.ie with retry
#'
#' @param uri Character. Full URL **or** a fragment path (prepended with
#'   `.oireachtas_data_url` automatically).
#' @keywords internal
.oir_get_xml_raw <- function(uri) {
  url <- if (grepl("^https?://", uri)) uri else paste0(.oireachtas_data_url, uri)

  max_retries <- getOption("oiReachtas.max_retries", 4L)
  wait        <- getOption("oiReachtas.retry_wait",  1)

  for (attempt in seq_len(max_retries + 1L)) {
    resp <- tryCatch(
      httr2::request(url)|>
        httr2::req_user_agent("oiReachtas R package")|>
        httr2::req_error(is_error = \(r) FALSE)|>   # handle HTTP errors manually below
        httr2::req_perform(),
      error = function(e) {
        if (attempt > max_retries) rlang::abort(
          paste0("Network error fetching XML: ", conditionMessage(e)),
          class = "oireachtas_network_error"
        )
        NULL
      }
    )

    if (is.null(resp)) {
      .oir_backoff(attempt, wait); wait <- wait * 2; next
    }

    status <- httr2::resp_status(resp)
    if (status < 400L) return(xml2::read_xml(httr2::resp_body_raw(resp)))

    retryable <- status %in% c(429L, 500L, 502L, 503L, 504L)
    if (retryable && attempt <= max_retries) {
      retry_after <- suppressWarnings(
        as.numeric(httr2::resp_header(resp, "retry-after") %||% NA_real_)
      )
      sleep_for <- if (length(retry_after) == 1L && !is.na(retry_after) && retry_after > 0) {
        retry_after
      } else {
        wait
      }
      message(sprintf("[oiReachtas] HTTP %d – retrying XML fetch in %.0fs", status, sleep_for))
      Sys.sleep(sleep_for); wait <- wait * 2; next
    }

    .oir_check_response(resp)
  }
}

#' @keywords internal
.oir_get_xml <- NULL  # overwritten in .onLoad

# ---------------------------------------------------------------------------
# .onLoad: wire up memoised wrappers
# ---------------------------------------------------------------------------

#' @keywords internal
.oir_cache <- NULL  # overwritten in .onLoad

.onLoad <- function(libname, pkgname) {
  max_age <- getOption("oiReachtas.cache_max_age", 3600)  # seconds; 0 = disabled

  cache <- if (max_age > 0) {
    cachem::cache_mem(max_age = max_age)
  } else {
    NULL
  }

  # Assign into the package namespace so other functions can reach it
  ns <- asNamespace(pkgname)
  assign(".oir_cache",       cache,                                     envir = ns)
  assign(".oir_get",         memoise::memoise(.oir_get_raw, cache = cache), envir = ns)
  assign(".oir_get_xml",     memoise::memoise(.oir_get_xml_raw, cache = cache), envir = ns)
}

# ---------------------------------------------------------------------------
# Shared response checker
# ---------------------------------------------------------------------------

#' @keywords internal
.oir_check_response <- function(resp) {
  if (httr2::resp_is_error(resp)) {
    status <- httr2::resp_status(resp)
    body <- tryCatch(
      httr2::resp_body_json(resp),
      error = function(e) list(message = httr2::resp_status_desc(resp))
    )
    msg <- body$message %||% httr2::resp_status_desc(resp)
    rlang::abort(
      paste0("Oireachtas API error [HTTP ", status, "]: ", msg),
      class = "oireachtas_api_error",
      status = status,
      body   = body
    )
  }
}

# ---------------------------------------------------------------------------
# Pagination helpers
# ---------------------------------------------------------------------------

#' @keywords internal
.oir_pagination <- function(limit = 50L, skip = 0L) {
  list(limit = as.integer(limit), skip = as.integer(skip))
}

#' @keywords internal
.oir_validate_date <- function(date, arg_name = "date") {
  if (is.null(date)) return(invisible(NULL))
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
    rlang::abort(
      paste0("`", arg_name, "` must be in YYYY-MM-DD format, got: ", date),
      class = "oireachtas_invalid_date"
    )
  }
  invisible(date)
}

#' @keywords internal
.oir_get_all <- function(endpoint, params = list(), limit = 50L, max_records = Inf) {
  results <- list()
  skip    <- 0L
  page    <- 1L

  repeat {
    message(sprintf("[oiReachtas] Fetching page %d (skip=%d, limit=%d)...",
                    page, skip, limit))

    page_params <- c(params, .oir_pagination(limit = limit, skip = skip))

    # Catch API errors mid-pagination: return partial results with a warning
    # rather than throwing and losing everything already collected.
    resp <- tryCatch(
      .oir_get(endpoint, page_params),
      error = function(e) {
        if (length(results) > 0L) {
          warning(sprintf(
            "[oiReachtas] Pagination stopped at page %d (skip=%d): %s\n  Returning %d items collected so far.",
            page, skip, conditionMessage(e), length(results)
          ))
          NULL
        } else {
          stop(e)   # nothing collected yet — re-throw so caller sees the error
        }
      }
    )
    if (is.null(resp)) break   # partial-results early exit

    items <- tryCatch(resp$results, error = function(e) {
      message(sprintf("[oiReachtas] Could not extract $results from response. Names: %s",
                      paste(names(resp), collapse = ", ")))
      list()
    })
    if (is.null(items)) items <- list()

    n_page <- length(items)
    results <- c(results, items)
    skip    <- skip + n_page
    message(sprintf("[oiReachtas] Page %d: %d items returned (total so far: %d)",
                    page, n_page, length(results)))
    page <- page + 1L

    if (n_page < limit || length(results) >= max_records) break
  }

  message(sprintf("[oiReachtas] Done – %d total items fetched.", length(results)))
  if (is.finite(max_records)) results <- results[seq_len(min(length(results), max_records))]
  results
}

# ---------------------------------------------------------------------------
# Windowed fetch helper (works around the API's 10,000-record page limit)
# ---------------------------------------------------------------------------

#' Fetch paginated data in consecutive date windows
#'
#' The Oireachtas API returns at most 10,000 records per query (Elasticsearch
#' `max_result_window`).  For high-volume endpoints like `/questions`, a
#' multi-year request will silently truncate.  This helper splits a date range
#' into consecutive windows of `window_months` months, calls `fetch_fn` for
#' each window with `date_start` / `date_end` injected, and row-binds the
#' results into a single tibble.
#'
#' @param fetch_fn A function that accepts `date_start` and `date_end`
#'   (ISO `"YYYY-MM-DD"`) plus any additional `...` arguments and returns a
#'   tibble.  Typically one of [get_questions()], [get_debates()], etc.
#' @param date_start Character. Overall start date (`"YYYY-MM-DD"`).
#' @param date_end Character. Overall end date (`"YYYY-MM-DD"`).
#' @param window_months Integer. Size of each window in months. Default `3`.
#' @param ... Additional arguments forwarded to `fetch_fn` on every call.
#'
#' @return A [tibble][tibble::tibble] combining all windows.
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch all written questions from 2019-2021 without hitting the 10k limit
#' qs <- fetch_windowed(
#'   get_questions,
#'   date_start    = "2019-01-01",
#'   date_end      = "2021-12-31",
#'   question_type = "written",
#'   limit         = 100,
#'   all_pages     = TRUE
#' )
#' }
fetch_windowed <- function(fetch_fn,
                           date_start,
                           date_end,
                           window_months = 3L,
                           ...) {
  .oir_validate_date(date_start, "date_start")
  .oir_validate_date(date_end,   "date_end")

  start <- as.Date(date_start)
  end   <- as.Date(date_end)

  if (start > end) {
    rlang::abort("`date_start` must be on or before `date_end`",
                 class = "oireachtas_invalid_date")
  }

  # Build window boundaries
  windows <- list()
  win_start <- start
  while (win_start <= end) {
    # Add window_months months then subtract one day
    win_end <- seq(win_start, by = paste(window_months, "months"), length.out = 2L)[2L] - 1L
    win_end <- min(win_end, end)
    windows <- c(windows, list(list(s = win_start, e = win_end)))
    win_start <- win_end + 1L
  }

  message(sprintf(
    "[oiReachtas] fetch_windowed: %d window(s) of ~%d months from %s to %s",
    length(windows), window_months, date_start, date_end
  ))

  results <- purrr::map(windows, function(w) {
    message(sprintf("[oiReachtas]   window %s → %s", w$s, w$e))
    tryCatch(
      fetch_fn(date_start = format(w$s), date_end = format(w$e), ...),
      error = function(e) {
        warning(sprintf(
          "[oiReachtas] Window %s → %s failed: %s  (skipping)",
          w$s, w$e, conditionMessage(e)
        ))
        tibble::tibble()
      }
    )
  })

  dplyr::bind_rows(results)
}
