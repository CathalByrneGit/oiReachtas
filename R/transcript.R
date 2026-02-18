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
.akn_ns <- c(akn = "http://docs.oasis-open.org/legaldocml/ns/akn/3.0")

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

#' Parse an Akoma Ntoso XML document into a tidy tibble
#' @keywords internal
.parse_akn <- function(xml_doc, by_para = TRUE, include_narrative = FALSE) {

  ns <- .akn_ns

  # ------------------------------------------------------------------
  # 1. Build speaker lookup from <meta>/<references>
  # ------------------------------------------------------------------
  ref_nodes  <- xml2::xml_find_all(xml_doc, ".//akn:references/akn:TLCPerson", ns)
  # Fall back to non-namespaced if document uses no namespace
  if (length(ref_nodes) == 0) {
    ref_nodes <- xml2::xml_find_all(xml_doc, ".//references/TLCPerson")
    ns        <- character(0)   # signal: use no-namespace XPaths below
  }

  speaker_lookup <- if (length(ref_nodes) > 0) {
    ids    <- xml2::xml_attr(ref_nodes, "id")
    hrefs  <- xml2::xml_attr(ref_nodes, "href")
    names_ <- xml2::xml_attr(ref_nodes, "showAs")
    stats::setNames(
      lapply(seq_along(ids), function(i) list(uri = hrefs[i], name = names_[i])),
      ids
    )
  } else {
    list()
  }

  # ------------------------------------------------------------------
  # 2. Helper: build xpath accounting for namespace presence
  # ------------------------------------------------------------------
  xp <- function(path) {
    if (length(ns) == 0) gsub("akn:", "", path) else path
  }

  # ------------------------------------------------------------------
  # 3. Walk every debateSection, collecting utterances
  # ------------------------------------------------------------------
  # We flatten all sections rather than recurse, collecting the heading
  # of the innermost section ancestor for each utterance.
  utterance_types <- c("speech", "question", "answer")
  if (include_narrative) utterance_types <- c(utterance_types, "narrative")

  all_sections <- xml2::xml_find_all(xml_doc, xp(".//akn:debateSection"), ns)
  if (length(all_sections) == 0) {
    all_sections <- xml2::xml_find_all(xml_doc, ".//debateSection")
  }

  speech_counter <- 0L
  rows <- list()

  for (section in all_sections) {
    section_id      <- xml2::xml_attr(section, "id") %||% NA_character_
    heading_node    <- xml2::xml_find_first(section, xp("akn:heading"), ns)
    section_heading <- if (!inherits(heading_node, "xml_missing")) {
      trimws(xml2::xml_text(heading_node))
    } else NA_character_

    # Only direct children that are utterances (avoid double-counting from
    # nested sections)
    children <- xml2::xml_children(section)
    child_names <- xml2::xml_name(children)

    for (i in seq_along(children)) {
      child      <- children[[i]]
      child_type <- child_names[[i]]
      # Strip namespace prefix if present (e.g., "akn:speech" → "speech")
      child_type <- sub("^.*:", "", child_type)

      if (!child_type %in% utterance_types) next

      speech_counter <- speech_counter + 1L

      if (child_type == "narrative") {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          section_id      = section_id,
          section_heading = section_heading,
          speech_no       = speech_counter,
          speech_type     = "narrative",
          speaker_ref     = NA_character_,
          speaker_uri     = NA_character_,
          speaker_name    = NA_character_,
          para_no         = if (by_para) 1L else NA_integer_,
          text            = trimws(xml2::xml_text(child))
        )
        next
      }

      # --- speech / question / answer ---
      by_raw     <- xml2::xml_attr(child, "by") %||% NA_character_
      ref_key    <- sub("^#", "", by_raw %||% "")
      speaker_lu <- speaker_lookup[[ref_key]]

      speaker_uri  <- speaker_lu$uri  %||% NA_character_
      speaker_name_lu <- speaker_lu$name %||% NA_character_

      # Prefer <from> text; fall back to lookup
      from_node    <- xml2::xml_find_first(child, xp("akn:from"), ns)
      speaker_name <- if (!inherits(from_node, "xml_missing")) {
        # Strip trailing colon and whitespace ("Mary Murphy:" → "Mary Murphy")
        trimws(sub(":+\\s*$", "", xml2::xml_text(from_node)))
      } else {
        speaker_name_lu
      }

      # Paragraphs (also span/inline text not wrapped in <p> is ignored)
      para_nodes <- xml2::xml_find_all(child, xp("akn:p"), ns)
      if (length(para_nodes) == 0) {
        para_nodes <- xml2::xml_find_all(child, "p")
      }

      if (length(para_nodes) == 0) next   # speech with no text

      if (by_para) {
        para_rows <- purrr::imap(para_nodes, function(p, idx) {
          tibble::tibble(
            section_id      = section_id,
            section_heading = section_heading,
            speech_no       = speech_counter,
            speech_type     = child_type,
            speaker_ref     = ref_key,
            speaker_uri     = speaker_uri,
            speaker_name    = speaker_name,
            para_no         = as.integer(idx),
            text            = trimws(xml2::xml_text(p))
          )
        })
        rows <- c(rows, para_rows)
      } else {
        full_text <- paste(
          trimws(purrr::map_chr(para_nodes, xml2::xml_text)),
          collapse = "\n"
        )
        rows[[length(rows) + 1L]] <- tibble::tibble(
          section_id      = section_id,
          section_heading = section_heading,
          speech_no       = speech_counter,
          speech_type     = child_type,
          speaker_ref     = ref_key,
          speaker_uri     = speaker_uri,
          speaker_name    = speaker_name,
          para_no         = NA_integer_,
          text            = full_text
        )
      }
    }
  }

  if (length(rows) == 0) return(tibble::tibble(
    section_id = character(), section_heading = character(),
    speech_no = integer(), speech_type = character(),
    speaker_ref = character(), speaker_uri = character(),
    speaker_name = character(), para_no = integer(), text = character()
  ))

  dplyr::bind_rows(rows)
}
