# Minimal Akoma Ntoso 3.0 XML fixture ----------------------------------------
# Uses the real AKN namespace as data.oireachtas.ie does.

.akn_fixture <- function() {
  xml2::read_xml('<?xml version="1.0" encoding="UTF-8"?>
<akomaNtoso xmlns="http://docs.oasis-open.org/legaldocml/ns/akn/3.0">
  <debate name="debate">
    <meta>
      <references source="#source">
        <TLCPerson id="MarySmith.D"  href="/ie/oireachtas/member/id/MarySmith.D"
                   showAs="Mary Smith"/>
        <TLCPerson id="JohnBrown.D"  href="/ie/oireachtas/member/id/JohnBrown.D"
                   showAs="John Brown"/>
        <TLCRole   id="Ceann-Comhairle"
                   href="/ie/oireachtas/role/ceannComhairle"
                   showAs="Ceann-Comhairle"/>
      </references>
    </meta>
    <debateBody>
      <debateSection id="dbsect_1" name="debate">
        <heading>Order of Business</heading>
        <speech by="#MarySmith.D" as="#Ceann-Comhairle">
          <from>Mary Smith:</from>
          <p>First paragraph of Mary\'s speech.</p>
          <p>Second paragraph of Mary\'s speech.</p>
        </speech>
        <question by="#JohnBrown.D" as="#member">
          <from>John Brown:</from>
          <p>My question text.</p>
        </question>
        <narrative>The House divided.</narrative>
        <answer by="#MarySmith.D" as="#Ceann-Comhairle">
          <from>Mary Smith:</from>
          <p>My answer.</p>
        </answer>
      </debateSection>
    </debateBody>
  </debate>
</akomaNtoso>')
}

# -----------------------------------------------------------------------------

test_that(".parse_akn returns a tibble with correct columns", {
  result <- oiReachtas:::.parse_akn(.akn_fixture())
  expect_s3_class(result, "data.frame")
  expected_cols <- c("section_id", "section_heading", "speech_no",
                     "speech_type", "speaker_ref", "speaker_uri",
                     "speaker_name", "para_no", "text")
  expect_true(all(expected_cols %in% names(result)))
})

test_that(".parse_akn by_para=TRUE returns one row per paragraph", {
  result <- oiReachtas:::.parse_akn(.akn_fixture(), by_para = TRUE)
  # Mary: 2 paras, John: 1 para, Mary again: 1 para  = 4 rows (narrative excluded)
  expect_equal(nrow(result), 4L)
  expect_equal(result$para_no, c(1L, 2L, 1L, 1L))
})

test_that(".parse_akn by_para=FALSE collapses paragraphs", {
  result <- oiReachtas:::.parse_akn(.akn_fixture(), by_para = FALSE)
  # 3 utterances (speech, question, answer), narrative excluded
  expect_equal(nrow(result), 3L)
  expect_true(all(is.na(result$para_no)))
  # Mary's collapsed text should contain both paragraphs separated by \n
  mary_text <- result$text[result$speaker_ref == "MarySmith.D" &
                             result$speech_type == "speech"]
  expect_true(grepl("\n", mary_text))
})

test_that(".parse_akn includes narrative rows when requested", {
  result <- oiReachtas:::.parse_akn(.akn_fixture(), include_narrative = TRUE)
  expect_true("narrative" %in% result$speech_type)
  narr <- result[result$speech_type == "narrative", ]
  expect_equal(trimws(narr$text), "The House divided.")
  expect_true(is.na(narr$speaker_ref))
})

test_that(".parse_akn resolves speaker names from <from>", {
  result <- oiReachtas:::.parse_akn(.akn_fixture())
  # <from> has trailing colon — must be stripped
  expect_equal(result$speaker_name[result$speaker_ref == "MarySmith.D"][1],
               "Mary Smith")
  expect_equal(result$speaker_name[result$speaker_ref == "JohnBrown.D"][1],
               "John Brown")
})

test_that(".parse_akn resolves speaker URIs from <references>", {
  result <- oiReachtas:::.parse_akn(.akn_fixture())
  expect_equal(result$speaker_uri[result$speaker_ref == "MarySmith.D"][1],
               "/ie/oireachtas/member/id/MarySmith.D")
})

test_that(".parse_akn populates section_id and section_heading", {
  result <- oiReachtas:::.parse_akn(.akn_fixture())
  expect_true(all(result$section_id == "dbsect_1"))
  expect_true(all(result$section_heading == "Order of Business"))
})

test_that(".parse_akn speech_type column reflects element name", {
  result <- oiReachtas:::.parse_akn(.akn_fixture())
  types <- unique(result$speech_type)
  expect_setequal(types, c("speech", "question", "answer"))
})

test_that(".parse_akn returns empty tibble for XML with no speeches", {
  empty_xml <- xml2::read_xml(
    '<akomaNtoso xmlns="http://docs.oasis-open.org/legaldocml/ns/akn/3.0">
       <debate name="debate">
         <debateBody>
           <debateSection id="s1" name="debate">
             <heading>Empty</heading>
           </debateSection>
         </debateBody>
       </debate>
     </akomaNtoso>'
  )
  result <- oiReachtas:::.parse_akn(empty_xml)
  expect_equal(nrow(result), 0L)
})

test_that("get_debate_text rejects NULL or empty uri", {
  expect_error(get_debate_text(NULL),  class = "oireachtas_invalid_param")
  expect_error(get_debate_text(""),    class = "oireachtas_invalid_param")
  expect_error(get_debate_text(NA_character_), class = "oireachtas_invalid_param")
})
