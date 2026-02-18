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
#' | [get_questions()] | Parliamentary questions |
#' | [get_divisions()] | Divisions (votes) |
#' | [get_division_votes()] | Individual member votes for a division |
#' | [get_legislation()] | Bills and Acts |
#' | [get_constituencies()] | Electoral constituencies |
#' | [get_parties()] | Political parties |
#' | [get_houses()] | Houses of the Oireachtas |
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
#' @importFrom httr GET http_error status_code http_status content user_agent accept_json
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom purrr map map_chr imap
#' @importFrom rlang abort `%||%`
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
