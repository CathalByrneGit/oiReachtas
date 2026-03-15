#' Retrieve Houses of the Oireachtas
#'
#' Returns a tibble describing the houses of the Oireachtas (Dáil and Seanad
#' sittings across different parliamentary terms).
#'
#' @param chamber Character. `"dail"`, `"seanad"`, or `""`. Default `""`.
#' @param house_no Integer or NULL. Specific house number. Default `NULL`.
#' @param chamber_id Character or NULL. Full chamber URI. Default `NULL`.
#' @param limit Integer. Records per page (default 50).
#' @param skip Integer. Records to skip. Default 0.
#' @param all_pages Logical. Fetch all pages. Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with one row per house.
#' @export
#'
#' @examples
#' \dontrun{
#' # All Dáil houses (terms)
#' get_houses(chamber = "dail")
#'
#' # Current Dáil
#' get_houses(chamber = "dail", house_no = 33)
#' }
get_houses <- function(chamber = "",
                       chamber_id = NULL,
                       limit = 50L,
                       skip = 0L,
                       all_pages = FALSE) {
  params <- list(
    chamber    = if (nchar(chamber) > 0) chamber else NULL,
    chamber_id = chamber_id
  )

  if (all_pages) {
    items <- .oir_get_all("/houses", params, limit = limit)
  } else {
    resp  <- .oir_get("/houses", c(params, .oir_pagination(limit, skip)))
    items <- resp$results%||% list()
  }

  .parse_houses(items)
}

#' @keywords internal
.parse_houses <- function(items) {
  if (length(items) == 0) return(tibble::tibble())

  rows <- purrr::map(items, function(item) {
    h <- item$house %||% item
    tibble::tibble(
      house_id       = .null_na(h$uri),
      chamber_type   = .null_na(h$chamberType),
      chamber_id     = .null_na(h$chamberCode),
      house_no       = .null_na(h$houseNo),
      show_as        = .null_na(h$showAs),
      date_start     = .null_na(h$dateRange$start %||% NA_character_),
      date_end       = .null_na(h$dateRange$end %||% NA_character_)
    )
  })

  dplyr::bind_rows(rows)
}
