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

  for (attempt in seq_len(max_retries + 1L)) {
    resp <- tryCatch(
      httr::GET(url,
                query  = params,
                httr::user_agent(ua),
                httr::accept_json()),
      error = function(e) {
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

    status <- httr::status_code(resp)

    # Success
    if (status < 400L) {
      return(jsonlite::fromJSON(
        httr::content(resp, as = "text", encoding = "UTF-8"),
        simplifyVector = FALSE
      ))
    }

    # Retryable: 429 (rate limit) or transient 5xx
    retryable <- status %in% c(429L, 500L, 502L, 503L, 504L)
    if (retryable && attempt <= max_retries) {
      retry_after <- suppressWarnings(
        as.numeric(httr::headers(resp)[["retry-after"]])
      )
      sleep_for <- if (!is.na(retry_after) && retry_after > 0) retry_after else wait
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
      httr::GET(url, httr::user_agent("oiReachtas R package"),
                httr::accept("application/xml")),
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

    status <- httr::status_code(resp)
    if (status < 400L) return(xml2::read_xml(httr::content(resp, as = "raw")))

    retryable <- status %in% c(429L, 500L, 502L, 503L, 504L)
    if (retryable && attempt <= max_retries) {
      retry_after <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
      sleep_for <- if (!is.na(retry_after) && retry_after > 0) retry_after else wait
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
    cachem::cache_null()
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
  if (httr::http_error(resp)) {
    status <- httr::status_code(resp)
    body <- tryCatch(
      jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")),
      error = function(e) list(message = httr::http_status(resp)$message)
    )
    msg <- body$message %||% httr::http_status(resp)$message
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

  repeat {
    page_params <- c(params, .oir_pagination(limit = limit, skip = skip))
    resp  <- .oir_get(endpoint, page_params)
    items <- tryCatch(resp$results$items, error = function(e) list())
    if (is.null(items)) items <- list()

    results <- c(results, items)
    skip    <- skip + length(items)

    if (length(items) < limit || length(results) >= max_records) break
  }

  if (is.finite(max_records)) results <- results[seq_len(min(length(results), max_records))]
  results
}
