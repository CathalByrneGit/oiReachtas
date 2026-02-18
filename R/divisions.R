#' Retrieve Parliamentary Divisions (Votes)
#'
#' Returns division (vote) records from the Dáil or Seanad. Each division
#' records how members voted on a motion or bill.
#'
#' @param chamber Character. `"dail"`, `"seanad"`, or `""`. Default `""`.
#' @param house_no Integer or NULL. Specific house number. Default `NULL`.
#' @param chamber_id Character or NULL. Full chamber URI. Default `NULL`.
#' @param date_start Character or NULL. Start date (`"YYYY-MM-DD"`).
#'   Default `NULL`.
#' @param date_end Character or NULL. End date (`"YYYY-MM-DD"`).
#'   Default `NULL`.
#' @param member_id Character or NULL. Filter divisions participated in by a
#'   specific member. Default `NULL`.
#' @param limit Integer. Records per page (default 50).
#' @param skip Integer. Records to skip. Default 0.
#' @param all_pages Logical. Fetch all pages. Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with one row per division.
#' @export
#'
#' @examples
#' \dontrun{
#' # All Dáil votes in 2023
#' get_divisions(chamber = "dail", date_start = "2023-01-01",
#'               date_end = "2023-12-31", all_pages = TRUE)
#'
#' # Votes a specific TD participated in
#' get_divisions(member_id = "/ie/oireachtas/member/id/MicheálMartin.D.1989-07-12")
#' }
get_divisions <- function(chamber = "",
                          house_no = NULL,
                          chamber_id = NULL,
                          date_start = NULL,
                          date_end = NULL,
                          member_id = NULL,
                          limit = 50L,
                          skip = 0L,
                          all_pages = FALSE) {
  .oir_validate_date(date_start, "date_start")
  .oir_validate_date(date_end, "date_end")

  params <- list(
    chamber    = if (nchar(chamber) > 0) chamber else NULL,
    house_no   = house_no,
    chamber_id = chamber_id,
    date_start = date_start,
    date_end   = date_end,
    member_id  = member_id
  )

  if (all_pages) {
    items <- .oir_get_all("/divisions", params, limit = limit)
  } else {
    resp  <- .oir_get("/divisions", c(params, .oir_pagination(limit, skip)))
    items <- resp$results$items %||% list()
  }

  .parse_divisions(items)
}

#' @keywords internal
.parse_divisions <- function(items) {
  if (length(items) == 0) return(tibble::tibble())

  rows <- purrr::map(items, function(item) {
    d <- item$division %||% item
    tibble::tibble(
      division_id     = .null_na(d$uri),
      date            = .null_na(d$date),
      chamber         = .null_na(d$house$showAs %||% NA_character_),
      house_no        = .null_na(d$house$houseNo %||% NA_character_),
      division_no     = .null_na(d$divisionNo),
      subject         = .null_na(d$subject$showAs %||% NA_character_),
      count_ta        = .null_na(d$tallies$ta %||% NA_integer_),
      count_nil       = .null_na(d$tallies$nil %||% NA_integer_),
      count_staon     = .null_na(d$tallies$staon %||% NA_integer_)
    )
  })

  dplyr::bind_rows(rows)
}

#' Retrieve Voting Record for a Specific Division
#'
#' Returns how each member voted in a given division.
#'
#' @param division_id Character. Division URI (from `get_divisions()$division_id`).
#'
#' @return A [tibble][tibble::tibble] with one row per member vote.
#' @export
#'
#' @examples
#' \dontrun{
#' divs <- get_divisions(chamber = "dail", date_start = "2023-06-01",
#'                       date_end = "2023-06-01")
#' get_division_votes(divs$division_id[1])
#' }
get_division_votes <- function(division_id) {
  resp  <- .oir_get("/divisions", list(division_id = division_id, limit = 1L))
  items <- resp$results$items %||% list()
  if (length(items) == 0) return(tibble::tibble())

  d <- items[[1]]$division %||% items[[1]]

  # Expand individual votes
  vote_sections <- list(
    ta    = d$votes$ta %||% list(),
    nil   = d$votes$nil %||% list(),
    staon = d$votes$staon %||% list()
  )

  rows <- purrr::imap(vote_sections, function(voters, vote_type) {
    purrr::map(voters, function(v) {
      tibble::tibble(
        division_id  = .null_na(d$uri),
        date         = .null_na(d$date),
        member_uri   = .null_na(v$member$uri %||% NA_character_),
        member_name  = .null_na(v$member$showAs %||% NA_character_),
        vote         = vote_type
      )
    })
  }) |> unlist(recursive = FALSE)

  dplyr::bind_rows(rows)
}
