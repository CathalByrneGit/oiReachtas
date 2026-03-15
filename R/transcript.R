# Oireachtas debates are published as Akoma Ntoso 3.0 XML at data.oireachtas.ie.
# The XML structure is:
#
#   <akomaNtoso xmlns="http://docs.oasis-open.org/legaldocml/ns/akn/3.0">
#     <debate>
#       <meta>
#         <references>
#           <TLCPerson id="MemberCode" href="/ie/oireachtas/member/id/..." showAs="Full Name"/>
#           <TLCRole   id="role"       href="..."                          showAs="Role Name"/>
#         </references>
#       </meta>
#       <debateBody>
#         <debateSection id="..." name="debate">
#           <heading>Section Title</heading>
#           <!-- speeches can be at any depth of nested debateSections -->
#           <speech by="#MemberCode" as="#role">
#             <from>Full Name:</from>
#             <p>First paragraph.</p>
#             <p>Second paragraph.</p>
#           </speech>
#           <question by="#MemberCode" as="#role">...</question>
#           <answer   by="#MemberCode" as="#role">...</answer>
#         </debateSection>
#       </debateBody>
#     </debate>
#   </akomaNtoso>

#' AKN 3.0 XML namespace
#' @keywords internal
# Updated to match your specific XML sample suffix (CSD13)
.akn_ns <- c(akn = "http://docs.oasis-open.org/legaldocml/ns/akn/3.0/CSD13")


# ---------------------------------------------------------------------------
# Public function
# ---------------------------------------------------------------------------

#' Retrieve and parse the full text of an Oireachtas debate transcript
#'
#' Fetches an [Akoma Ntoso](http://www.akomantoso.org/) 3.0 XML transcript
#' from `data.oireachtas.ie` and returns it as a tidy tibble with one row per
#' speech paragraph (or one row per speech when `by_para = FALSE`).
#'
#' The `formats_xml` column returned by [get_debates()] contains the URI
#' fragment to pass to this function.
#'
#' @param uri Character. Full URL **or** a fragment path (e.g.
#'   `"/ie/oireachtas/debate/..."`) from `get_debates()$formats_xml`.
#' @param by_para Logical. If `TRUE` (default) returns one row per paragraph.
#'   If `FALSE` collapses paragraphs into a single `text` string per speech
#'   (useful for document-level sentiment analysis).
#' @param include_narrative Logical. Include procedural `<narrative>` elements
#'   as rows with `speech_type = "narrative"`. Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with columns:
#'   \describe{
#'     \item{`section_id`}{`debateSection` XML id attribute}
#'     \item{`section_heading`}{Heading of the innermost containing section}
#'     \item{`speech_no`}{Sequential speech/narrative index in the document}
#'     \item{`speech_type`}{`"speech"`, `"question"`, `"answer"`, or `"narrative"`}
#'     \item{`speaker_ref`}{Member code from the `by` attribute (NA for narrative)}
#'     \item{`speaker_uri`}{Full member URI from `<meta>/<references>` (NA if not found)}
#'     \item{`speaker_name`}{Display name from `<from>` element or `<references>`}
#'     \item{`para_no`}{Paragraph number within speech (NA when `by_para = FALSE`)}
#'     \item{`text`}{Text content}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' debates <- get_debates(chamber = "dail", date_start = "2023-01-17",
#'                        date_end = "2023-01-17")
#' transcript <- get_debate_text(debates$formats_xml[1])
#'
#' # One row per speech (for document-level analysis)
#' by_speech <- get_debate_text(debates$formats_xml[1], by_para = FALSE)
#' }
get_debate_text <- function(uri,
                            by_para            = TRUE,
                            include_narrative  = FALSE) {
  if (is.null(uri) || is.na(uri) || !nzchar(uri)) {
    rlang::abort("`uri` must be a non-empty character string",
                 class = "oireachtas_invalid_param")
  }

  xml_doc <- .oir_get_xml(uri)
  .parse_akn(xml_doc, by_para = by_para, include_narrative = include_narrative)
}

# ---------------------------------------------------------------------------
# Internal parser
# ---------------------------------------------------------------------------

#' Parse an Akoma Ntoso 3.0 XML debate transcript into a tidy tibble
#'
#' @param xml_doc An xml2 document object.
#' @param by_para Logical. If TRUE, returns one row per paragraph. If FALSE, collapses speech.
#' @param include_narrative Logical. If TRUE, includes summary and narrative tags.
#'
#' @return A tibble with columns: section_id, section_heading, speech_no, speech_type, 
#'         speaker_ref, speaker_uri, speaker_name, para_no, and text.
#' @importFrom xml2 xml_find_all xml_find_first xml_attr xml_text xml_ns xml_children xml_name
#' @importFrom dplyr bind_rows % > %
#' @importFrom purrr map_chr map2
#' @importFrom tibble tibble
#' @keywords internal
.parse_akn <- function(xml_doc, by_para = TRUE, include_narrative = FALSE) {
  
  # 1. Dynamic Namespace Detection
  # Oireachtas XML often fluctuates between .../akn/3.0 and .../akn/3.0/CSD13
  # Robust Namespace Extraction
  all_ns <- xml2::xml_ns(xml_doc)
  akn_uri <- as.character(all_ns[grepl("legaldocml", all_ns)])[1]
  
  if (is.na(akn_uri)) {
    # Fallback if no Akoma Ntoso namespace is found
    ns <- character(0) 
  } else {
    ns <- c(akn = akn_uri)
  }
  # 2. Build speaker lookup from <meta>/<references>
  # Using eId because Oireachtas Akoma Ntoso uses eId for references
  ref_nodes <- xml2::xml_find_all(xml_doc, ".//akn:references/akn:TLCPerson", ns)
  
  speaker_lookup <- if (length(ref_nodes) > 0) {
    ids    <- xml2::xml_attr(ref_nodes, "eId")
    hrefs  <- xml2::xml_attr(ref_nodes, "href")
    names_ <- xml2::xml_attr(ref_nodes, "showAs")
    
    stats::setNames(
      purrr::map2(hrefs, names_, ~list(uri = .x, name = .y)),
      ids
    )
  } else {
    list()
  }
  
  # 3. Define Utterance Types
  utterance_types <- c("speech", "question", "answer")
  if (include_narrative) utterance_types <- c(utterance_types, "summary", "narrative")
  
  # 4. Walk Sections
  all_sections <- xml2::xml_find_all(xml_doc, ".//akn:debateSection", ns)
  speech_counter <- 0L
  rows <- list()
  
  for (section in all_sections) {
    section_id      <- xml2::xml_attr(section, "eId")
    heading_node    <- xml2::xml_find_first(section, "./akn:heading", ns)
    
    # Clean heading text (removes timestamp text often found inside headings)
    section_heading <- if (!inherits(heading_node, "xml_missing")) {
      base_text <- xml2::xml_text(heading_node)
      trimws(sub("\\d{2}:\\d{2}:\\d{2}.*$", "", base_text))
    } else NA_character_
    
    # Get immediate children to avoid double-counting nested sections
    children <- xml2::xml_children(section)
    
    for (child in children) {
      child_type <- xml2::xml_name(child)
      if (!child_type %in% utterance_types) next
      
      speech_counter <- speech_counter + 1L
      
      # Speaker Metadata
      by_raw  <- xml2::xml_attr(child, "by")
      ref_key <- sub("^#", "", by_raw %||% "")
      speaker_lu <- speaker_lookup[[ref_key]]
      
      # Priority: <from> tag text > Meta Lookup
      from_node <- xml2::xml_find_first(child, "./akn:from", ns)
      speaker_name <- if (!inherits(from_node, "xml_missing")) {
        trimws(sub(":+\\s*$", "", xml2::xml_text(from_node)))
      } else {
        speaker_lu$name %||% NA_character_
      }
      
      # Extract Paragraphs
      para_nodes <- xml2::xml_find_all(child, "./akn:p", ns)
      if (length(para_nodes) == 0) para_nodes <- list(child) # Handle block-level text
      
      if (by_para) {
        for (idx in seq_along(para_nodes)) {
          rows[[length(rows) + 1L]] <- tibble::tibble(
            section_id      = section_id,
            section_heading = section_heading,
            speech_no       = speech_counter,
            speech_type     = child_type,
            speaker_ref     = ref_key,
            speaker_uri     = speaker_lu$uri %||% NA_character_,
            speaker_name    = speaker_name,
            para_no         = as.integer(idx),
            text            = trimws(xml2::xml_text(para_nodes[[idx]]))
          )
        }
      } else {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          section_id      = section_id,
          section_heading = section_heading,
          speech_no       = speech_counter,
          speech_type     = child_type,
          speaker_ref     = ref_key,
          speaker_uri     = speaker_lu$uri %||% NA_character_,
          speaker_name    = speaker_name,
          para_no         = NA_integer_,
          text            = paste(trimws(purrr::map_chr(para_nodes, xml2::xml_text)), collapse = "\n")
        )
      }
    }
  }
  
  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}
