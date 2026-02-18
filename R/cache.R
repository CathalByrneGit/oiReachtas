#' Clear the oiReachtas in-memory cache
#'
#' Removes all cached API and XML responses for the current R session.
#' Useful when you need fresh data without restarting R.
#'
#' The cache is automatically populated whenever [get_members()],
#' [get_debates()], [get_debate_text()], etc. make network requests.
#' Its lifetime is controlled by `options(oiReachtas.cache_max_age = 3600)`
#' (default 1 hour). Set to `0` before loading the package to disable caching
#' entirely.
#'
#' @return Invisibly `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' oir_cache_clear()
#' }
oir_cache_clear <- function() {
  .oir_cache$reset()
  message("[oiReachtas] Cache cleared.")
  invisible(NULL)
}

#' Inspect the oiReachtas in-memory cache
#'
#' Returns a list describing the current state of the session cache.
#'
#' @return A list with:
#'   \describe{
#'     \item{`n_entries`}{Number of cached responses.}
#'     \item{`keys`}{Character vector of cache keys.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' oir_cache_info()
#' }
oir_cache_info <- function() {
  keys <- .oir_cache$keys()
  list(n_entries = length(keys), keys = keys)
}
