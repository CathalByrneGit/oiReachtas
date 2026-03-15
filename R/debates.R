#' Retrieve Oireachtas Debates
#'
#' Returns a tibble of parliamentary debates (Dáil, Seanad, or committee)
#' matching the given filters. The API returns metadata for each debate;
#' the actual transcript can be retrieved via [get_debate_record()].
#'
#' @param chamber Character. `"dail"`, `"seanad"`, or `""` for both.
#'   Default `""`.
#' @param house_no Integer or NULL. Specific house number. Default `NULL`.
#' @param chamber_id Character or NULL. Full chamber URI. Default `NULL`.
#' @param date_start Character or NULL. Start date (`"YYYY-MM-DD"`).
#'   Default `NULL`.
#' @param date_end Character or NULL. End date (`"YYYY-MM-DD"`).
#'   Default `NULL`.
#' @param member_id Character or NULL. Member URI to filter debates by speaker.
#'   Default `NULL`.
#' @param debate_id Character or NULL. Specific debate URI. Default `NULL`.
#' @param limit Integer. Records per page (default 50).
#' @param skip Integer. Records to skip. Default 0.
#' @param all_pages Logical. Fetch all pages automatically. Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with one row per debate section.
#' @export
#'
#' @examples
#' \dontrun{
#' # Dáil debates in a date range
#' get_debates(chamber = "dail", date_start = "2023-01-01", date_end = "2023-03-31")
#'
#' # Debates by a specific member
#' get_debates(member_id = "/ie/oireachtas/member/id/MicheálMartin.D.1989-07-12")
#' }
get_debates <- function(chamber = "",
                        chamber_id = NULL,
                        chamber_type = NULL,
                        member_id = NULL,
                        debate_id = NULL,
                        date_start = NULL,
                        date_end = NULL,
                        limit = 50L,
                        skip = 0L,
                        all_pages = FALSE) {
  
  .oir_validate_date(date_start, "date_start")
  .oir_validate_date(date_end, "date_end")
  
  params <- list(
    chamber    = if (nchar(chamber) > 0) chamber else NULL,
    chamber_type = chamber_type,
    chamber_id = chamber_id,
    member_id = member_id,
    debate_id = debate_id,
    date_end =date_end,
    date_start = date_start,
    debate_id =debate_id
  )
  


  if (all_pages) {
    items <- .oir_get_all("/debates", params, limit = limit)
  } else {
    resp  <- .oir_get("/debates", c(params, .oir_pagination(limit, skip)))
    items <- resp$results %||% list()
  }

  .parse_debates(items)
}

#' Retrieve a Single Debate Record (Full Transcript Metadata)
#'
#' Returns the full record for a single debate, including format URIs that
#' can be used to download the XML (Akoma Ntoso) transcript.
#'
#' @param debate_id Character. The debate URI (from `get_debates()$debate_uri`).
#' @param date Character. The date of the debate (`"YYYY-MM-DD"`).
#'
#' @return A list with full debate metadata and format URIs.
#' @export
#'
#' @examples
#' \dontrun{
#' debates <- get_debates(chamber = "dail", date_start = "2023-01-17", date_end = "2023-01-17")
#' get_debate_record(debates$debate_id[1])
#' }
get_debate_record <- function(debate_id, date = NULL) {
  .oir_validate_date(date, "date")

  params <- list(
    debate_id  = debate_id,
    date       = date
  )

  .oir_get("/debates", Filter(Negate(is.null), params))
}

#' @keywords internal
.parse_debates <- function(items) {
  if (length(items) == 0) return(tibble::tibble())

  rows <- purrr::map(items, function(item) {
    
    d <- item$debateRecord %||% item
    

    tibble::tibble(
      debate_id        = .null_na(d$uri),
      debate_type      = .null_na(d$debateType),
      chamber          = .null_na(d$chamber$showAs %||% d$house$showAs %||% NA_character_),
      house_no         = .null_na(d$house$houseNo %||% NA_character_),
      committee_code = .null_na(d$house$comitteeCode %||% NA_character_),
      date             = .null_na(d$date),
      counts_questions = .null_na(d$counts$questionCount %||% NA_integer_),
      counts_bill = .null_na(d$counts$billCount %||% NA_integer_),
      counts_countributor = .null_na(d$counts$contributorCount %||% NA_integer_),
      
      formats_xml      = .null_na(d$formats$xml$uri %||% NA_character_)
      
    )
  })

  dplyr::bind_rows(rows)
}
