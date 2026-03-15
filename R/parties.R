#' Retrieve Political Parties
#'
#' Returns a tibble of political parties represented in the Oireachtas.
#'
#' @param chamber Character. `"dail"`, `"seanad"`, or `""`. Default `""`.
#' @param house_no Integer or NULL. Specific house number. Default `NULL`.
#' @param chamber_id Character or NULL. Full chamber URI. Default `NULL`.
#' @param limit Integer. Records per page (default 50).
#' @param skip Integer. Records to skip. Default 0.
#' @param all_pages Logical. Fetch all pages. Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with one row per party.
#' @export
#'
#' @examples
#' \dontrun{
#' get_parties(chamber = "dail", house_no = 33)
#' }
get_parties <- function(chamber = "",
                        house_no = NULL,
                        chamber_id = NULL,
                        limit = 50L,
                        skip = 0L,
                        all_pages = FALSE) {
  params <- list(
    chamber    = if (nchar(chamber) > 0) chamber else NULL,
    house_no   = house_no,
    chamber_id = chamber_id
  )

  if (all_pages) {
    items <- .oir_get_all("/parties", params, limit = limit)
  } else {
    resp  <- .oir_get("/parties", c(params, .oir_pagination(limit, skip)))
    items <- resp$results %||% list()
  }

  .parse_parties(items)
}

#' @keywords internal
.parse_parties <- function(items) {
  if (length(items) == 0) return(tibble::tibble())

  rows <- purrr::map(items, function(item) {
    p <- item
    tibble::tibble(
      party_id   = .null_na(p$party$uri),
      party_code = .null_na(p$party$partyCode),
      name       = .null_na(p$party$showAs %||% p$party$name),
      chamber    = .null_na(p$house$showAs %||% NA_character_),
      house_no   = .null_na(p$house$houseNo %||% NA_character_)
    )
  })

  dplyr::bind_rows(rows)
}
