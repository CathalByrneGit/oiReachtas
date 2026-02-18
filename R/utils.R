#' @keywords internal
.oireachtas_base_url <- "https://api.oireachtas.ie/v1"

#' Make a GET request to the Oireachtas API
#'
#' @param endpoint Character. API endpoint path (e.g., `"/members"`).
#' @param params Named list of query parameters.
#' @param user_agent Character. User-agent string sent with every request.
#'
#' @return A parsed list representing the JSON response body.
#' @keywords internal
.oir_get <- function(endpoint, params = list(), user_agent = "oiReachtas R package") {
  url <- paste0(.oireachtas_base_url, endpoint)

  # Remove NULL values from params
  params <- Filter(Negate(is.null), params)

  resp <- httr::GET(
    url,
    query = params,
    httr::user_agent(user_agent),
    httr::accept_json()
  )

  .oir_check_response(resp)

  jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                     simplifyVector = FALSE)
}

#' Check an httr response for errors
#'
#' @param resp An `httr` response object.
#' @keywords internal
.oir_check_response <- function(resp) {
  if (httr::http_error(resp)) {
    status <- httr::status_code(resp)
    body <- tryCatch(
      jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")),
      error = function(e) list(message = httr::http_status(resp)$message)
    )
    msg <- if (!is.null(body$message)) body$message else httr::http_status(resp)$message
    rlang::abort(
      paste0("Oireachtas API error [HTTP ", status, "]: ", msg),
      class = "oireachtas_api_error",
      status = status,
      body = body
    )
  }
}

#' Build common pagination parameters
#'
#' @param limit Integer. Maximum records to return (default 50, max 100).
#' @param skip Integer. Number of records to skip (for pagination).
#' @keywords internal
.oir_pagination <- function(limit = 50L, skip = 0L) {
  list(limit = as.integer(limit), skip = as.integer(skip))
}

#' Validate a date string in YYYY-MM-DD format
#'
#' @param date Character or NULL.
#' @param arg_name Character. Argument name for the error message.
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

#' Retrieve all pages of a paginated endpoint
#'
#' Automatically pages through results and returns a combined list.
#'
#' @param endpoint Character. API endpoint path.
#' @param params Named list of fixed query parameters (excluding `skip`/`limit`).
#' @param limit Integer. Page size (default 50).
#' @param max_records Integer or Inf. Hard cap on total records fetched.
#'
#' @return A list of result items combined across all pages.
#' @keywords internal
.oir_get_all <- function(endpoint, params = list(), limit = 50L, max_records = Inf) {
  results <- list()
  skip <- 0L

  repeat {
    page_params <- c(params, .oir_pagination(limit = limit, skip = skip))
    resp <- .oir_get(endpoint, page_params)

    # Results are typically under resp$results$items
    items <- tryCatch(resp$results$items, error = function(e) list())
    if (is.null(items)) items <- list()

    results <- c(results, items)
    skip <- skip + length(items)

    if (length(items) < limit || length(results) >= max_records) break
  }

  if (is.finite(max_records)) results <- results[seq_len(min(length(results), max_records))]
  results
}
