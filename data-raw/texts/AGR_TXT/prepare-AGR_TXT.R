# ENVIRONMENTAL AGREEMENTS TEXTS script preparation

# Due to the specificities of the text database,
# the usual script preparation format has been adapted.

# Step one: create a consolidated version of manyenviron agreements database
AGR_TXT <- manydata::consolidate(manyenviron::agreements,
                                 "any",
                                 "any",
                                 resolve = "coalesce",
                                 key = "manyID")

# Step two: extract treaty texts from IEADB website
# This requires the original IEADB dataset with the ieadbID column
IEADB_original <- readr::read_csv("data-raw/agreements/IEADB/treaties.csv")
IEADB_original <- dplyr::as_tibble(IEADB_original) %>%
  dplyr::rename(ieadbID = `IEA# (click for add'l info)`,
                TreatyText = `Treaty Text`) %>%
  dplyr::select(ieadbID, TreatyText) %>%
  dplyr::filter(TreatyText == "Treaty Text**") %>%
  dplyr::arrange(ieadbID)
IEADB_original$ieadbID <- as.character(IEADB_original$ieadbID)

# Then, rvest package is used to web scrap IEADB treaty texts pages
base <- "https://iea.uoregon.edu/treaty-text/"
IEADB_original$TreatyText <- lapply(IEADB_original$ieadbID,
                                    function(s) tryCatch(rvest::read_html(paste0(base, s)) %>%
                                                           rvest::html_nodes(".even") %>%
                                                           rvest::html_text(),
                                                         error = function(e) {
                                                           as.character("Not found")
                                                           } %>%
                                                           paste(collapse = ",")))
# A source column is also added
IEADB_original$Source <- lapply(IEADB_original$ieadbID,
                                function(s) tryCatch(rvest::read_html(paste0(base, s)) %>%
                                                       rvest::html_nodes(".views-field-views-conditional") %>%
                                                       rvest::html_text(),
                                                     error = function(e) {
                                                       as.character("Not found")
                                                       } %>%
                                                       paste(collapse = ",")))

# Step three: join IEADB text column with the consolidated version of
# manyenviron
AGR_TXT <- dplyr::left_join(AGR_TXT,
                            IEADB_original,
                            by = "ieadbID")
AGR_TXT <- as_tibble(AGR_TXT) %>%
  dplyr::select(treatyID, ieadbID, gnevarID, ecolexID,
                manyID, Title, Beg, TreatyText, Source)

# Step four: complement the dataset with ECOLEX treaty texts (TO BE IMPROVED)
# Filter observations with text = NULL and with an ecolexID
ecolex_text <- AGR_TXT %>%
  dplyr::filter(!is.na(ecolexID) & TreatyText == "NULL")

ecolex_text$ecolexID2 <- stringr::str_remove_all(ecolex_text$ecolexID, "-")

# Use pdftools package to extract treaty texts
base <- "http://www.ecolex.org/server2neu.php/libcat/docs/TRE/Full/En/"
ecolex_text$TreatyText <- lapply(ecolex_text$ecolexID,
                                 function(s) tryCatch(pdftools::pdf_text(paste0(base, s, ".pdf")),
                                                      error = function(e) {
                                                        as.character("Not found")
                                                        }))
ecolex_text$TreatyTextb <- lapply(ecolex_text$ecolexID2,
                                  function(s) tryCatch(pdftools::pdf_text(paste0(base, s, ".pdf")),
                                                       error = function(e) {
                                                         as.character("Not found")
                                                         }))


base2 = "http://www.ecolex.org/server2neu.php/libcat/docs/TRE/Full/Other/"
ecolex_text$TreatyText2 <- lapply(ecolex_text$ecolexID,
                                  function(s) tryCatch(pdftools::pdf_text(paste0(base2, s, ".pdf")),
                                                       error = function(e) {
                                                         as.character("Not found")
                                                         }))
ecolex_text$TreatyText2b <- lapply(ecolex_text$ecolexID2,
                                   function(s) tryCatch(pdftools::pdf_text(paste0(base2, s, ".pdf")),
                                                        error = function(e) {
                                                          as.character("Not found")
                                                          }))

# base3 = "http://www.ecolex.org/server2neu.php/libcat/docs/TRE/Full/Fr/"
# ecolex_text$TreatyText3 <- lapply(ecolex_text$ecolexID,
#                                   function(s) tryCatch(pdftools::pdf_text(paste0(base3, s, ".pdf")),
#                                                        error = function(e){as.character("Not found")}))
# ecolex_text$TreatyText3b <- lapply(ecolex_text$ecolexID2,
#                                    function(s) tryCatch(pdftools::pdf_text(paste0(base3, s, ".pdf")),
#                                                         error = function(e){as.character("Not found")}))
#
# base4 = "http://www.ecolex.org/server2neu.php/libcat/docs/TRE/Full/Sp/"
# ecolex_text$TreatyText4 <- lapply(ecolex_text$ecolexID,
#                                   function(s) tryCatch(pdftools::pdf_text(paste0(base4, s, ".pdf")),
#                                                        error = function(e){as.character("Not found")}))
# ecolex_text$TreatyText4b <- lapply(ecolex_text$ecolexID2,
#                                    function(s) tryCatch(pdftools::pdf_text(paste0(base4, s, ".pdf")),
#                                                         error = function(e){as.character("Not found")}))

ecolex_text$TreatyText <- dplyr::na_if(ecolex_text$TreatyText, "Not found")
ecolex_text$TreatyTextb <- dplyr::na_if(ecolex_text$TreatyTextb, "Not found")
ecolex_text$TreatyText2 <- dplyr::na_if(ecolex_text$TreatyText2, "Not found")
ecolex_text$TreatyText2b <- dplyr::na_if(ecolex_text$TreatyText2b, "Not found")

ecolex_text$Text <- dplyr::coalesce(ecolex_text$TreatyText,
                                    ecolex_text$TreatyTextb,
                                    ecolex_text$TreatyText2,
                                    ecolex_text$TreatyText2b)

ecolex_text <- ecolex_text %>%
  dplyr::filter(!is.na(Text)) %>%
  dplyr::select(ecolexID, Text)

ecolex_text$Source <- "ECOLEX"

# Step five: join ECOLEX texts to the agreements dataset
AGR_TXT <- dplyr::left_join(AGR_TXT,  ecolex_text, by = "ecolexID")
AGR_TXT$Source.x <- dplyr::na_if(AGR_TXT$Source.x, "NULL")
AGR_TXT$Source.y <- dplyr::na_if(AGR_TXT$Source.y, "NULL")

AGR_TXT$TreatyText <- dplyr::na_if(AGR_TXT$TreatyText, "NULL")
AGR_TXT$Text <- dplyr::na_if(AGR_TXT$Text, "NULL")
AGR_TXT$Text <- dplyr::coalesce(AGR_TXT$TreatyText, AGR_TXT$Text)

AGR_TXT <- dplyr::as_tibble(AGR_TXT) %>%
  dplyr::rename(Source = `Source.x`) %>%
  dplyr::select(manyID, Title, Beg, Text,
                Source, ieadbID, gnevarID, ecolexID)

# Step six: export data into rda format 2 possible solutions
manypkgs::export_data(AGR_TXT, database = "texts", URL = "NA")
# !Make sure to deletethe database before exporting the data,
# and do not re-write the texts documentation.
# usethis::use_data(AGR_TXT, internal = T, overwrite = T)
