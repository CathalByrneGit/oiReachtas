#' Retrieve Constituencies
#'
#' Returns a tibble of Oireachtas electoral constituencies.
#'
#' @param chamber Character. `"dail"`, `"seanad"`, or `""`. Default `""`.
#' @param house_no Integer or NULL. Specific house number. Default `NULL`.
#' @param chamber_id Character or NULL. Full chamber URI. Default `NULL`.
#' @param limit Integer. Records per page (default 50).
#' @param skip Integer. Records to skip. Default 0.
#' @param all_pages Logical. Fetch all pages. Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with one row per constituency.
#' @export
#'
#' @examples
#' \dontrun{
#' get_constituencies(chamber = "dail", house_no = 33)
#' }
get_constituencies <- function(chamber = "",
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
    items <- .oir_get_all("/constituencies", params, limit = limit)
  } else {
    resp  <- .oir_get("/constituencies", c(params, .oir_pagination(limit, skip)))
    items <- resp$results %||% list()
  }

  .parse_constituencies(items)
}

#' @keywords internal
.parse_constituencies <- function(items) {
  if (length(items) == 0) return(tibble::tibble())

  rows <- purrr::map(items, function(item) {
    c <- item$constituency %||% item
    print(c)
    tibble::tibble(
      panel_id   = .null_na(c$uri),
      name              = .null_na(c$showAs %||% NA_character_ ),
      panel_type = .null_na(c$representType),
      chamber           = .null_na(item$house$houseCode %||% NA_character_),
      house_no          = .null_na(item$house$houseNo %||% NA_character_),
    )
  })

  dplyr::bind_rows(rows)
}
