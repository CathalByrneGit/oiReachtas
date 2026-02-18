#' Retrieve Legislation (Bills and Acts)
#'
#' Returns a tibble of bills and acts from the Oireachtas legislation database.
#'
#' @param chamber Character. `"dail"`, `"seanad"`, or `""`. Default `""`.
#' @param house_no Integer or NULL. Specific house number. Default `NULL`.
#' @param bill_id Character or NULL. Specific bill URI. Default `NULL`.
#' @param bill_no Integer or NULL. Bill number. Default `NULL`.
#' @param bill_year Integer or NULL. Year of the bill. Default `NULL`.
#' @param bill_status Character or NULL. Status filter, e.g. `"Current"`,
#'   `"Enacted"`, `"Defeated"`, `"Withdrawn"`, `"Lapsed"`. Default `NULL`.
#' @param date_start Character or NULL. Start date (`"YYYY-MM-DD"`).
#'   Default `NULL`.
#' @param date_end Character or NULL. End date (`"YYYY-MM-DD"`).
#'   Default `NULL`.
#' @param member_id Character or NULL. Filter bills sponsored by member.
#'   Default `NULL`.
#' @param limit Integer. Records per page (default 50).
#' @param skip Integer. Records to skip. Default 0.
#' @param all_pages Logical. Fetch all pages. Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with one row per bill/act.
#' @export
#'
#' @examples
#' \dontrun{
#' # Current bills before the Dáil
#' get_legislation(bill_status = "Current", chamber = "dail")
#'
#' # All enacted legislation in a year
#' get_legislation(bill_status = "Enacted", bill_year = 2022, all_pages = TRUE)
#' }
get_legislation <- function(chamber = "",
                            house_no = NULL,
                            bill_id = NULL,
                            bill_no = NULL,
                            bill_year = NULL,
                            bill_status = NULL,
                            date_start = NULL,
                            date_end = NULL,
                            member_id = NULL,
                            limit = 50L,
                            skip = 0L,
                            all_pages = FALSE) {
  .oir_validate_date(date_start, "date_start")
  .oir_validate_date(date_end, "date_end")

  params <- list(
    chamber     = if (nchar(chamber) > 0) chamber else NULL,
    house_no    = house_no,
    bill_id     = bill_id,
    bill_no     = bill_no,
    bill_year   = bill_year,
    bill_status = bill_status,
    date_start  = date_start,
    date_end    = date_end,
    member_id   = member_id
  )

  if (all_pages) {
    items <- .oir_get_all("/legislation", params, limit = limit)
  } else {
    resp  <- .oir_get("/legislation", c(params, .oir_pagination(limit, skip)))
    items <- resp$results$items %||% list()
  }

  .parse_legislation(items)
}

#' @keywords internal
.parse_legislation <- function(items) {
  if (length(items) == 0) return(tibble::tibble())

  rows <- purrr::map(items, function(item) {
    b <- item$bill %||% item
    tibble::tibble(
      bill_id        = .null_na(b$uri),
      bill_no        = .null_na(b$billNo),
      bill_year      = .null_na(b$billYear),
      bill_type      = .null_na(b$billType),
      bill_status    = .null_na(b$status),
      title          = .null_na(b$shortTitleEn %||% b$longTitleEn),
      title_ga       = .null_na(b$shortTitleGa %||% b$longTitleGa),
      chamber        = .null_na(b$originHouse$showAs %||% NA_character_),
      sponsor_name   = .null_na(b$sponsors[[1]]$sponsor$showAs %||%
                                  b$sponsors[[1]]$sponsor$member$showAs %||%
                                  NA_character_),
      date_introduced = .null_na(b$datePublished %||% NA_character_),
      act_no         = .null_na(b$act$actNo %||% NA_character_),
      act_year       = .null_na(b$act$actYear %||% NA_character_)
    )
  })

  dplyr::bind_rows(rows)
}
