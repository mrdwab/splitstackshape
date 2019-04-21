library(rodham)

context("All tests")

Sys.setlocale("LC_TIME", "C")

test_that("test all", {
  # search
  emails <- search_emails()
  expect_equal(nrow(emails), 29444)
  expect_equal(ncol(emails), 9)
  expect_equal(names(emails), c("docID", "docDate", "to", "from",
                                "originalTo", "originalFrom", "subject",
                                "interesting", "not_interesting"))
  # edges
  expect_error(edges_emails())
  expect_equal(nrow(edges_emails(emails)), 795)
  expect_equal(ncol(edges_emails(emails)), 3)
  expect_equal(ncol(edges_emails(emails, "subject")), 4)
  expect_equal(names(edges_emails(emails, "subject", "docDate")),
               c("from", "to", "subject", "docDate", "freq"))
  # errors get
  expect_error(get_emails())

  #  1 email
  contents <- structure(list(C05765907 = structure(list(docID = "C05768606",
                                                        docDate = "2014-08-12", to = "", from = "", originalTo = "",
                                                        originalFrom = "", subject = "DRAFT AGENDA FOR 34TH ANNUAL COLP CONFERENCE APRIL 19.DOC",
                                                        interesting = NA_character_, not_interesting = NA_character_,
                                                        content = c("UNCLASSIFIED U.S. Department of State Case No. F-2014-20439 Doc No. C05765907 Date: 08/31/2015",
                                                                    "From: Sent: To: Subject:", "Sullivan, Jacob J <SullivanJJ@state.gov> Friday, December 4, 2009 3:31 AM",
                                                                    "Iran", "RELEASE IN PART B5", "The EU meets in the coming days, and we are hoping for a strong public - and private - position on Iran. Bill has identified 5 countries that need touching to help drive a good outcome:",
                                                                    "I know Huma has discussed with you, but a 2-minute discussion with each that underscores the key points reflected on your card would do the trick, if you can swing it.",
                                                                    "Tx. Also, the intervention, with your modifications, turned out well. The process, in this case, did not generate a good • enough product -- I tried to make it clearer and stronger this morning and your amendments helped a lot.",
                                                                    "UNCLASSIFIED U.S. Department of State Case No. F-2014-20439 Doc No. C05765907 Date: 08/31/2015"
                                                        )), .Names = c("docID", "docDate", "to", "from", "originalTo",
                                                                       "originalFrom", "subject", "interesting", "not_interesting",
                                                                       "content")), C05765911 = structure(list(docID = "C05778466",
                                                                                                               docDate = "2012-12-17", to = "Hillary Clinton", from = "Melanne Verveer",
                                                                                                               originalTo = "H", originalFrom = "PVerveer", subject = "FROM ",
                                                                                                               interesting = NA_character_, not_interesting = NA_character_,
                                                                                                               content = c("UNCLASSIFIED U.S. Department of State Case No. F-2014-20439 Doc No. C05765911 Date: 08/31/2015",
                                                                                                                           "From: Sent: To: Subject:", "Mills, Cheryl D <MillsCD@state.gov> Friday, December 4, 2009 9:35 AM H FW: Frm SecState Trip to Middle East",
                                                                                                                           "RELEASE IN PART B6", "From: Macmanus, Joseph E Sent: Friday, December 04, 2009 9:09 AM To: Mills, Cheryl D; Abedin, Huma; Burns, William J; Kennedy, Patrick F; Boswell, Eric J,. Feltman, Jeffrey D Subject: Frm SecState Trip to Middle East",
                                                                                                                           "From: Colby Cooper [mailto Sent: Friday, December 04, 2009 8:56 AM To: Macmanus, Joseph E Cc: Anne Lyons; Colby Cooper Subject: CR Trip to Middle East",
                                                                                                                           "Joe -", "Just wanted to give you a heads-up that former Secretary Rice will be traveling to the UAE and Saudi Arabia December 11-14. This is purely a business trip, as you can expect she may conduct courtesy calls with government officials. The desk and Posts have been notified as a courtesy. I have direct contact with the RSOs as a contingency. Security and Transportation, at their insistence, are being provided by the host countries.",
                                                                                                                           "Hope all is well.", "Best, Colby", "Colby J. Cooper Email: Phone: Fax: (251) 928-0271 Cell:",
                                                                                                                           "UNCLASSIFIED U.S. Department of State Case No. F-2014-20439 Doc No. C05765911 Date: 08/31/2015"
                                                                                                               )), .Names = c("docID", "docDate", "to", "from", "originalTo",
                                                                                                                              "originalFrom", "subject", "interesting", "not_interesting",
                                                                                                                              "content"))), .Names = c("C05765907", "C05765911"), class = "rodham")


  # methods
  expect_s3_class(contents, "rodham")
  expect_type(get_content(contents), "list")
  expect_equal(nrow(get_date(contents)), 2)
  expect_equal(nrow(get_com(contents)), 2)
  expect_equal(nrow(get_or(contents)), 2)
  expect_equal(nrow(get_subject(contents)), 2)
  expect_equal(nrow(get_interest(contents)), 2)
  expect_equal(length(get_content(contents)), 2)
  cont <- get_content(contents)
  expect_equal(length(clean_content(content = cont)[[1]]), 5)
})
