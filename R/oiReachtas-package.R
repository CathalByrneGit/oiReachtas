#' oiReachtas: R Interface to the Houses of the Oireachtas Open Data API
#'
#' Provides a tidy interface to the Houses of the Oireachtas (Irish Parliament)
#' Open Data API. Retrieve parliamentary members, debates, questions, divisions
#' (votes), legislation, constituencies, parties, and houses as tibbles ready
#' for analysis.
#'
#' @section Main functions:
#' | Function | Description |
#' |---|---|
#' | [get_members()] | Members (TDs and Senators) |
#' | [get_debates()] | Parliamentary debates |
#' | [get_debate_record()] | Full record for a single debate |
#' | [get_debate_text()] | Parsed full text of a debate transcript (Akoma Ntoso XML) |
#' | [get_questions()] | Parliamentary questions |
#' | [get_divisions()] | Divisions (votes) |
#' | [get_division_votes()] | Individual member votes for a division |
#' | [get_legislation()] | Bills and Acts |
#' | [get_constituencies()] | Electoral constituencies |
#' | [get_parties()] | Political parties |
#' | [get_houses()] | Houses of the Oireachtas |
#'
#' @section Caching:
#' All API and XML responses are cached in memory for the duration of the R
#' session using [memoise](https://memoise.r-lib.org/). The default cache
#' lifetime is 1 hour; change it before loading the package:
#'
#' ```r
#' options(oiReachtas.cache_max_age = 1800)  # 30 minutes
#' options(oiReachtas.cache_max_age = 0)      # disable caching
#' library(oiReachtas)
#' ```
#'
#' Use [oir_cache_clear()] to flush the cache mid-session, and
#' [oir_cache_info()] to inspect it.
#'
#' @section Rate limiting:
#' The package automatically retries requests that receive HTTP 429 (Too Many
#' Requests) or transient 5xx errors, using exponential back-off. Defaults:
#'
#' ```r
#' options(oiReachtas.max_retries = 4)   # total retry attempts
#' options(oiReachtas.retry_wait  = 1)   # initial wait in seconds
#' ```
#'
#' @section API information:
#' The Oireachtas Open Data API is publicly accessible at
#' <https://api.oireachtas.ie/>. No authentication is required. Data are
#' licensed under the [Oireachtas (Open Data) PSI
#' Licence](https://www.oireachtas.ie/en/open-data/).
#'
#' Debate transcripts conform to the [Akoma
#' Ntoso](http://www.akomantoso.org/) international XML standard and are
#' served from <https://data.oireachtas.ie/>.
#'
#' @name oiReachtas-package
#' @aliases oiReachtas
"_PACKAGE"

## usethis namespace: start
#' @importFrom httr GET http_error status_code http_status content user_agent accept_json accept headers
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom purrr map map_chr imap
#' @importFrom rlang abort `%||%`
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_attr xml_text xml_children xml_name
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem cache_null
#' @importFrom stats setNames
## usethis namespace: end
NULL

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Coerce NULL to NA (scalar)
#' @keywords internal
.null_na <- function(x, na = NA_character_) {
  if (is.null(x) || length(x) == 0) return(na)
  x
}
