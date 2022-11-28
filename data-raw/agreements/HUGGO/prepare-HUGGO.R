# HUGGO Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many packages universe.

# Stage one: Collecting data
HUGGO1 <- readr::read_csv("data-raw/agreements/HUGGO/EnvGov Nodes-Table 1 VERS2.csv")
HUGGO2 <- readr::read_csv("data-raw/agreements/HUGGO/gnevar.csv")
HUGGO4 <- readr::read_csv("data-raw/agreements/HUGGO/GENG v1.2 (31.10.2015).csv")
HUGGO6 <- readr::read_delim("data-raw/agreements/HUGGO/MEA.Nodes v1.0.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'HUGGO' object until the object created
# below (in stage three) passes all the tests.
HUGGO1 <- as_tibble(HUGGO1)  %>%
  dplyr::mutate(AgreementType = dplyr::recode(T, G = "A", M = "E", "T" = "Q",
                                              D = "V", R = "W", N = "X",
                                              U = "Y")) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DocSign),
                        End = messydates::as_messydate(DocEnd),
                        Force = messydates::as_messydate(DocForce),
                        gengID = GENG) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::rename(DocType = L) %>%
  dplyr::rename(GeogArea = J) %>%
  dplyr::select(gengID, Title, Beg, End, DocType, GeogArea, AgreementType,
                Signature, Force) %>%
  dplyr::arrange(Beg)

# Add treatyID column
HUGGO1$treatyID <- manypkgs::code_agreements(HUGGO1)

# # Clean HUGGO 2
HUGGO2 <- as_tibble(HUGGO2) %>%
  dplyr::mutate(AgreementType = dplyr::recode(T, G = "A", M = "E", "T" = "Q",
                                              D = "V", R = "W", N = "X",
                                              U = "Y")) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DocSign),
                        End = messydates::as_messydate(DocEnd),
                        Force = messydates::as_messydate(DocForce),
                        gengID = GENG) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::rename(DocType = L) %>%
  dplyr::rename(GeogArea = J) %>%
  dplyr::select(gengID, Title, Beg, End, DocType, GeogArea, AgreementType,
                Signature, Force) %>%
  dplyr::arrange(Beg)

# Add treatyID column
HUGGO2$treatyID <- manypkgs::code_agreements(HUGGO2)

# Clean HUGGO4
HUGGO4$Parties <- paste0(HUGGO4$Country.x, "-", HUGGO4$Country.y)
HUGGO4 <- as_tibble(HUGGO4) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DocDate),
                        Force = messydates::as_messydate(InForce),
                        gengID = `GENG-B`) %>%
  dplyr::mutate(End = messydates::as_messydate(End)) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::select(gengID, Title, Beg, Signature, Force, End, Parties)

# Add treatyID column
HUGGO4$treatyID <- manypkgs::code_agreements(HUGGO4)

# MEA nodes (MGENG dataset) - HUGGO6
# For some more information about the variables and codes,
# please see the documentation in the data-raw folder.
HUGGO6 <- as_tibble(HUGGO6) %>%
  dplyr::select(-'...1') %>%
  dplyr::rename(verified = '...2',
                ecolexID = ECOLEX.ID,
                ieaID = IEA.ID,
                gengID = GENG.ID,
                MEA_type = Type,
                subject_ecolex = Subject.x,
                subject_iea = Subject.y,
                url = Text) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DocSign),
                        Force = messydates::as_messydate(DocForce)) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title),
                verified = case_when(verified == "%" ~ "verified",
                                     verified == "?" ~ "not verified"),
                Coded = as.character(Coded),
                Lit = as.character(Lit),
                Data = as.character(Data)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Beg)

# Add treatyID column
HUGGO6$treatyID <- manypkgs::code_agreements(HUGGO6)

# Join the datasets together
## fixed minor coding issue with HUGGO before joining data (28-09-2022)
# HUGGO$Source <- as.character(HUGGO$Source)

# Create a HUGGO "database" to apply consolidate()
HUGGO <- list(HUGGO1, HUGGO2, HUGGO4, HUGGO6)

# Join the datasets together
HUGGO <- manydata::consolidate(HUGGO, row = "any", cols = "any",
                               resolve = "coalesce", key = "treatyID") %>% 
  dplyr::distinct()

# Add manyID column
CIESIN <- manyenviron::agreements$CIESIN
ECOLEX <- manyenviron::agreements$ECOLEX
HEIDI <- manyenviron::agreements$HEIDI
IEADB <- manyenviron::agreements$IEADB
manyID <- manypkgs::condense_agreements(manyenviron::agreements,
                                        var = c(CIESIN$treatyID,
                                                ECOLEX$treatyID,
                                                HEIDI$treatyID,
                                                IEADB$treatyID,
                                                HUGGO$treatyID))

HUGGO <- dplyr::left_join(HUGGO, manyID, by = "treatyID")

# Extract treaty texts 
# Create a texs "database" to apply consolidate()
texts <- list(HUGGO, IEADB, ECOLEX, CIESIN, HEIDI)
texts <- manydata::consolidate(texts, "any", "any",
                               resolve = "coalesce", key = "manyID")

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

# Step three: join IEADB text column with the consolidated version of manyenviron
texts <- dplyr::left_join(texts,
                          IEADB_original,
                          by = "ieadbID")
texts <- as_tibble(texts) %>%
  dplyr::select(treatyID, ieadbID, gengID, ecolexID,
                manyID, Title, Beg, TreatyText, Source, url)

# Step four: complement the dataset with ECOLEX treaty texts (TO BE IMPROVED)
# Filter observations with text = NULL and with an ecolexID
ecolex_text <- texts %>%
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


base2 <- "http://www.ecolex.org/server2neu.php/libcat/docs/TRE/Full/Other/"
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

ecolex_text$url <- ifelse(!is.na(ecolex_text$TreatyText), "http://www.ecolex.org/server2neu.php/libcat/docs/TRE/Full/En/",
                                    ifelse(!is.na(ecolex_text$TreatyTextb), "http://www.ecolex.org/server2neu.php/libcat/docs/TRE/Full/En/",
                                           ifelse(!is.na(ecolex_text$TreatyText2), "http://www.ecolex.org/server2neu.php/libcat/docs/TRE/Full/Other/",
                                                  ifelse(!is.na(ecolex_text$TreatyText2b), "http://www.ecolex.org/server2neu.php/libcat/docs/TRE/Full/Other/",
                                                         NA))))

ecolex_text <- ecolex_text %>%
  dplyr::filter(!is.na(Text)) %>%
  dplyr::select(ecolexID, Title, Beg, Text, url) %>%
  dplyr::rename(TreatyText = Text)

# Step five: join ECOLEX texts to the agreements dataset
texts <- dplyr::left_join(texts,  ecolex_text, by = c("ecolexID", "Title", "Beg","TreatyText", "url"))

# Step six: Clean texts
# manypkgs includes several functions that should help cleaning
# and standardizing your data.
# Please see the vignettes or website for more details.
texts <- texts %>%
  dplyr::mutate(TreatyText = manypkgs::standardise_treaty_text(TreatyText))

texts <- dplyr::as_tibble(texts) %>%
  dplyr::select(manyID, Title, Beg, TreatyText, url, Source)

# Bind data
HUGGO <- dplyr::left_join(HUGGO, texts, by = c("manyID", "Title", "Beg", "url"))

# reorder variables
HUGGO <- dplyr::relocate(HUGGO, manyID, Title, Beg, End, Signature,
                         Force, AgreementType, DocType, GeogArea,
                         gengID, ieaID, ecolexID, treatyID)

# make sure all vars are correctly coded as NA if necessary
HUGGO <- HUGGO %>% 
  dplyr::mutate(across(everything(), ~stringr::str_replace_all(., "^NA$",
                                                               NA_character_))) %>% 
  dplyr::distinct() %>% 
  mutate(Signature = messydates::as_messydate(Signature),
         Force = messydates::as_messydate(Force),
         Beg = messydates::as_messydate(Beg),
         End = messydates::as_messydate(End)) %>% 
  dplyr::distinct() %>%
  dplyr::arrange(Beg)

# Standardising treaty texts
HUGGO <- HUGGO %>%
  dplyr::mutate(MainText = ifelse(!grepl("APPENDIX | ANNEX", unlist(TreatyText), perl = T),
                                  TreatyText,
                                  stringr::str_remove(HUGGO$TreatyText,
                                                      "APPENDIX.* | ANNEX.*")))
HUGGO <- HUGGO %>%
  dplyr::mutate(AppendixText = ifelse(grepl("APPENDIX", unlist(TreatyText), perl = T),
                                      stringr::str_extract(HUGGO$TreatyText,
                                                           "APPENDIX.*"),
                                      NA))
HUGGO <- HUGGO %>%
  dplyr::mutate(AnnexText = ifelse(grepl("ANNEX", unlist(TreatyText), perl = T),
                                   stringr::str_extract(HUGGO$TreatyText,
                                                        "ANNEX.*"),
                                   NA))

# Checked_HUGGO and Confirmed_HUGGO variables to track progress on manually correcting entries
# Checked_HUGGO: code 1 when the entire row's observations have been verified and updated
# Confirmed_HUGGO: list variables for which the observation could be verified and confirmed.
# Eg. List 'Signature' in `Confirmed_HUGGO` if the Signature date was found and verified in the treaty text or in a manual online search.
HUGGO$Checked_HUGGO <- NA
HUGGO$Confirmed_HUGGO <- NA

# Add Changes var to log changes that are manually added for each agreement.
HUGGO$Changes <- NA

# Stage three: Connecting data
# Next run the following line to make HUGGO available
# within the package.
manypkgs::export_data(HUGGO, database = "agreements",
                      URL = "Hand-coded data by the GGO team")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure
# adherence to certain standards. You can hit Cmd-Shift-T (Mac)
# or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and
# fill in as much detail about the variables etc as possible.

# To reduce size of text data stored in package:
# 1. after exporting HUGGO to agreements database, 
# load 'agreements.rda' in environment.
# 2. Delete 'agreements.rda' in 'data' folder.
# 3. Run `usethis::use_data(agreements, internal = F, overwrite = T, compress = "xz")`
# to save compressed text data.
