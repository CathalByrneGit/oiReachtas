#' Retrieve Oireachtas Members
#'
#' Returns a tibble of current or historical Oireachtas members (TDs and
#' Senators) matching the given filters.
#'
#' @param chamber Character. `"dail"`, `"seanad"`, or `""` for both
#'   (default `""`).
#' @param house_no Integer or NULL. Specific house number
#'   (e.g., `33` for the 33rd Dáil). Default `NULL` (all houses).
#' @param date_start Character or NULL. Filter members active on or after this
#'   date (`"YYYY-MM-DD"`). Default `NULL`.
#' @param date_end Character or NULL. Filter members active on or before this
#'   date (`"YYYY-MM-DD"`). Default `NULL`.
#' @param member_id Character or NULL. Full member URI to retrieve a specific
#'   member (e.g., `"/ie/oireachtas/member/id/MicheálMartin.D.1989-07-12"`).
#'   Default `NULL`.
#' @param limit Integer. Maximum records per page (default 50, max 100).
#' @param skip Integer. Records to skip (for manual pagination). Default 0.
#' @param all_pages Logical. If `TRUE` automatically fetches all pages.
#'   Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with one row per member.
#' @export
#'
#' @examples
#' \dontrun{
#' # All current Dáil members
#' get_members(chamber = "dail", house_no = 33)
#'
#' # All members across both houses
#' get_members(all_pages = TRUE)
#' }
get_members <- function(chamber = "",
                        house_no = NULL,
                        date_start = NULL,
                        date_end = NULL,
                        member_id = NULL,
                        limit = 50L,
                        skip = 0L,
                        all_pages = FALSE) {
  .oir_validate_date(date_start, "date_start")
  .oir_validate_date(date_end, "date_end")

  params <- list(
    chamber      = if (nchar(chamber) > 0) chamber else NULL,
    house_no     = house_no,
    date_start   = date_start,
    date_end     = date_end,
    member_id    = member_id
  )

  if (all_pages) {
    items <- .oir_get_all("/members", params, limit = limit)
  } else {
    resp  <- .oir_get("/members", c(params, .oir_pagination(limit, skip)))
    items <- resp$results$items %||% list()
  }

  .parse_members(items)
}

#' @keywords internal
.parse_members <- function(items) {
  if (length(items) == 0) return(tibble::tibble())

  rows <- purrr::map(items, function(item) {
    m <- item$member
    tibble::tibble(
      member_id       = .null_na(m$memberCode),
      full_name       = .null_na(m$fullName),
      first_name      = .null_na(m$firstName),
      last_name       = .null_na(m$lastName),
      gender          = .null_na(m$gender),
      date_of_birth   = .null_na(m$dateOfBirth),
      uri             = .null_na(m$uri),
      party_code      = .null_na(m$memberships[[1]]$membership$parties[[1]]$party$partyCode %||% NA_character_),
      party_name      = .null_na(m$memberships[[1]]$membership$parties[[1]]$party$showAs %||% NA_character_),
      constituency    = .null_na(m$memberships[[1]]$membership$represents[[1]]$represent$showAs %||% NA_character_),
      chamber         = .null_na(m$memberships[[1]]$membership$house$chamberType %||% NA_character_),
      house_no        = .null_na(m$memberships[[1]]$membership$house$houseNo %||% NA_character_),
      date_start      = .null_na(m$memberships[[1]]$membership$dateRange$start %||% NA_character_),
      date_end        = .null_na(m$memberships[[1]]$membership$dateRange$end %||% NA_character_)
    )
  })

  dplyr::bind_rows(rows)
}
