#' Retrieve Parliamentary Questions
#'
#' Returns parliamentary questions (oral or written) submitted in the Dáil or
#' Seanad, filtered by the given criteria.
#'
#' @param chamber Character. `"dail"`, `"seanad"`, or `""`. Default `"dail"`.
#' @param house_no Integer or NULL. Specific house number. Default `NULL`.
#' @param chamber_id Character or NULL. Full chamber URI. Default `NULL`.
#' @param date_start Character or NULL. Start date (`"YYYY-MM-DD"`).
#'   Default `NULL`.
#' @param date_end Character or NULL. End date (`"YYYY-MM-DD"`).
#'   Default `NULL`.
#' @param member_id Character or NULL. Member URI of the questioner.
#'   Default `NULL`.
#' @param question_type Character or NULL. `"oral"` or `"written"`.
#'   Default `NULL` (both).
#' @param limit Integer. Records per page (default 50).
#' @param skip Integer. Records to skip. Default 0.
#' @param all_pages Logical. Fetch all pages. Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with one row per question.
#' @export
#'
#' @examples
#' \dontrun{
#' # Written questions in January 2023
#' get_questions(question_type = "written", date_start = "2023-01-01",
#'               date_end = "2023-01-31")
#'
#' # Oral questions from a specific member
#' get_questions(member_id = "/ie/oireachtas/member/id/MaryLou.McDonald.D.2002-05-17",
#'               question_type = "oral")
#' }
get_questions <- function(chamber = "dail",
                          house_no = NULL,
                          chamber_id = NULL,
                          date_start = NULL,
                          date_end = NULL,
                          member_id = NULL,
                          question_type = NULL,
                          limit = 50L,
                          skip = 0L,
                          all_pages = FALSE) {
  .oir_validate_date(date_start, "date_start")
  .oir_validate_date(date_end, "date_end")

  if (!is.null(question_type) && !question_type %in% c("oral", "written")) {
    rlang::abort("`question_type` must be 'oral', 'written', or NULL",
                 class = "oireachtas_invalid_param")
  }

  params <- list(
    chamber       = if (nchar(chamber) > 0) chamber else NULL,
    house_no      = house_no,
    chamber_id    = chamber_id,
    date_start    = date_start,
    date_end      = date_end,
    member_id     = member_id,
    question_type = question_type
  )

  if (all_pages) {
    items <- .oir_get_all("/questions", params, limit = limit)
  } else {
    resp  <- .oir_get("/questions", c(params, .oir_pagination(limit, skip)))
    items <- resp$results$items %||% list()
  }

  .parse_questions(items)
}

#' @keywords internal
.parse_questions <- function(items) {
  if (length(items) == 0) return(tibble::tibble())

  rows <- purrr::map(items, function(item) {
    q <- item$question %||% item
    tibble::tibble(
      question_id   = .null_na(q$questionURI %||% q$uri),
      question_type = .null_na(q$questionType),
      question_no   = .null_na(q$questionNo),
      date          = .null_na(q$date),
      member_uri    = .null_na(q$askedBy[[1]]$showAs %||% NA_character_),
      member_name   = .null_na(q$askedBy[[1]]$showAs %||% NA_character_),
      department    = .null_na(q$to[[1]]$showAs %||% NA_character_),
      show_as       = .null_na(q$showAs),
      formats_xml   = .null_na(
        purrr::map_chr(q$formats %||% list(), ~ .x$uri %||% NA_character_)[1]
      )
    )
  })

  dplyr::bind_rows(rows)
}
