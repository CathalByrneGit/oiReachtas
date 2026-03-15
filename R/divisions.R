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
                          chamber_type = NULL,
                          chamber_id = NULL,
                          date_start = NULL,
                          date_end = NULL,
                          member_id = NULL,
                          debate_id = NULL,
                          vote_id = NULL,
                          outcome = NULL,# Carried Lost
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
    member_id  = member_id,
    outcome = outcome,
    vote_id = vote_id,
    debate_id =debate_id
  )

  if (all_pages) {
    items <- .oir_get_all("/divisions", params, limit = limit)
  } else {
    resp  <- .oir_get("/divisions", c(params, .oir_pagination(limit, skip)))
    items <- resp$results %||% list()
  }

  .parse_divisions(items)
}

#' @keywords internal
.parse_divisions <- function(items) {
  if (length(items) == 0) return(tibble::tibble())

  
  
  
  
  rows <- purrr::map(items, function(item) {
    
    
    d <- item$division

    members_id_ta <- sapply(d$tallies$taVotes$members,
                            \(x) x$member$memberCode %||% NA_character_)|>
      paste(collapse = ';')
    
    members_id_nil <- sapply(d$tallies$nilVotes$members,
                             \(x) x$member$memberCode %||% NA_character_)|>
      paste(collapse = ';')
    
    members_id_staon <- sapply(d$tallies$staonVotes$members,
                               \(x) x$member$memberCode %||% NA_character_)|>
      paste(collapse = ';')
    
    tibble::tibble(
      division_id     = .null_na(d$uri),
      date            = .null_na(d$date),
      outcome = .null_na(d$outcome),
      is_bill = d$isBill,
      title = .null_na(d$debate$showAs%||% NA_character_),
      chamber         = .null_na(d$chamber$showAs %||% NA_character_),
      house_no        = .null_na(d$house$houseNo %||% NA_character_),
      vote_id        = .null_na(d$voteId %||% NA_character_),
      category = .null_na(d$category %||% NA_character_),
      subject = .null_na(d$subject$showAs %||% NA_character_),
      debate_title = .null_na(d$debate$showAs %||% NA_character_),
      debate_xml = .null_na(d$debate$formats$xml$uri %||% NA_character_),
      committeeCode   = .null_na(d$house$committeeCode %||% NA_character_),
      count_ta        = .null_na(d$tallies$taVotes$tally %||% NA_integer_),
      count_nil       = .null_na(d$tallies$nilVotes$tally %||% NA_integer_),
      count_staon     = .null_na(d$tallies$staonVotes$tally %||% NA_integer_),
      member_id_ta   = .null_na(members_id_ta %||% NA_character_),
      members_id_nil = .null_na(members_id_nil %||% NA_character_),
      members_id_staon = .null_na(members_id_staon %||% NA_character_)
    )
  })

  dplyr::bind_rows(rows)
}

