

#' Retrieve Oireachtas Members Core Identity
#'
#' @description
#' `get_members` fetches high-level identity data for members of the Oireachtas 
#' (TDs and Senators). This function focuses on the "person" rather than 
#' their specific political roles or historical party shifts.
#'
#' @details
#' This function returns one row per unique member ID. To get a detailed 
#' history of which parties a member belonged to or which constituencies 
#' they represented over time, use [get_member_parties()].
#'
#' @param chamber Character. Filter by house type: `"dail"` or `"seanad"`. 
#'   Default `""` returns both.
#' @param house_no Integer. The specific sitting of the house (e.g., `33` for 
#'   the 33rd Dáil).
#' @param date_start,date_end Character. ISO 8601 date strings (`"YYYY-MM-DD"`) 
#'   to filter members active during a specific period.
#' @param member_id Character. The unique URI or code for a specific member 
#'   (e.g., `"/ie/oireachtas/member/id/Micheál-Martin.D.1989-07-12"`).
#' @param party_code Character. Filter results by a specific political party 
#'   code (e.g., `"FF"`, `"FG"`, `"SF"`).
#' @param const_code Character. Filter by constituency code.
#' @param limit Integer. Number of records to return per request. Max 100.
#' @param skip Integer. Number of records to skip for manual pagination.
#' @param all_pages Logical. If `TRUE`, recursively fetches all available 
#'   pages of data.
#'
#' @return A [tibble][tibble::tibble] containing:
#' \itemize{
#'   \item `member_id`: Unique identifier for the member.
#'   \item `full_name`: The member's full display name.
#'   \item `first_name` / `last_name`: Separated name components.
#'   \item `gender`: The member's gender.
#'   \item `uri`: The Oireachtas Open Data API resource URI.
#' }
#' @export
#' Retrieve Oireachtas Members
get_members <- function(chamber = "",
                        house_no = NULL,
                        date_start = NULL,
                        date_end = NULL,
                        member_id = NULL,
                        party_code = NULL,
                        const_code = NULL,
                        fuzzy_name_search = NULL,
                        limit = 50L,
                        skip = 0L,
                        all_pages = FALSE) {
  
  .oir_validate_date(date_start, "date_start")
  .oir_validate_date(date_end, "date_end")
  
  if(!is.null(member_id) && !grepl('/ie/oireachtas/member/id',member_id)){
    
    
    member_id <- file.path('/ie/oireachtas/member/id',member_id)
    
  }
  
  params <- list(
    chamber = if (nchar(chamber) > 0) chamber else NULL,
    house_no = house_no,
    date_start = date_start,
    date_end = date_end,
    member_id = member_id,
    party_code = party_code,
    const_code = const_code,
    fuzzy_name_search = fuzzy_name_search
  )
  
  if (all_pages) {
    items <- .oir_get_all("/members", params, limit = limit)
  } else {
    resp <- .oir_get("/members", c(params, .oir_pagination(limit, skip)))
    items <- resp$results %||% list()
  }
  
  .parse_members_core(items)
}


#' Retrieve Member Party and Constituency History
#'
#' @description
#' `get_member_parties` extracts the granular history of a member's 
#' career, including their party affiliations, the houses they sat in, 
#' and the constituencies they represented.
#'
#' @details
#' Because members often serve multiple terms, switch parties, or move 
#' between the Dáil and the Seanad, this function returns a "long" 
#' format tibble. A single member may have multiple rows representing 
#' different periods of their career.
#' 
#' @section Data Relationship:
#' This data can be joined to the output of [get_members()] using the 
#' `member_id` column.
#'
#' @param chamber Character. `"dail"` or `"seanad"`.
#' @param house_no Integer. Specific house number.
#' @param member_id Character. Specific member URI.
#' @param all_pages Logical. Whether to fetch all results across pagination.
#'
#' @return A [tibble][tibble::tibble] containing:
#' \itemize{
#'   \item `member_id`: Foreign key to the member record.
#'   \item `party_code` / `party_name`: Political affiliation details.
#'   \item `constituency`: The area the member represented.
#'   \item `chamber`: The house type (Dáil or Seanad).
#'   \item `house_no`: The specific sitting number.
#'   \item `start_date` / `end_date`: The validity period for this 
#'     specific membership/party combination.
#' }
#' @export
get_member_parties <- function(chamber = "", house_no = NULL, member_id = NULL, all_pages = FALSE) {
  
  if(!is.null(member_id) && !grepl('/ie/oireachtas/member/id',member_id)){
    
    
    member_id <- file.path('/ie/oireachtas/member/id',member_id)
  }
  
  # Re-uses the same logic to fetch data
  params <- list(chamber = chamber, house_no = house_no, member_id = member_id)
  
  
  
  if (all_pages) {
    items <- .oir_get_all("/members", params)
  } else {
    resp <- .oir_get("/members", params)
    items <- resp$results %||% list()
  }
  
  .parse_member_parties(items)
}


#' @keywords internal
.parse_members_core <- function(items) {
  if (length(items) == 0) return(tibble::tibble())
  
  purrr::map_df(items, function(item) {
    m <- item$member
    tibble::tibble(
      member_code     = .null_na(m$memberCode),
      full_name     = .null_na(m$fullName),
      first_name    = .null_na(m$firstName),
      last_name     = .null_na(m$lastName),
      gender        = .null_na(m$gender),
      date_of_death = .null_na(m$dateOfDeath),
      uri           = .null_na(m$uri)
    )
  })
}

#' @keywords internal
.parse_member_parties <- function(items) {
  if (length(items) == 0) return(tibble::tibble())
  
  purrr::map_df(items, function(item) {
    m <- item$member
    m_id <- .null_na(m$memberCode)
    
    # Iterate through every membership the person has had
    purrr::map_df(m$memberships, function(mem_wrapper) {
      ms <- mem_wrapper$membership
      
      # Extract constituency (representing)
      const <- ms$represents[[1]]$represent$showAs %||% NA_character_
      
      # Iterate through every party they belonged to during THIS membership
      purrr::map_df(ms$parties, function(p_wrapper) {
        p <- p_wrapper$party
        tibble::tibble(
          member_id   = m_id,
          party_code  = .null_na(p$partyCode),
          party_name  = .null_na(p$showAs),
          constituency = const,
          chamber     = .null_na(ms$house$chamberType),
          house_no    = .null_na(ms$house$houseNo),
          start_date  = .null_na(p$dateRange$start|>
                                   as.Date()),
          end_date    = .null_na(p$dateRange$end|>
                                   as.Date(),na = as.Date(NA_character_))
        )
      })
    })
  })
}