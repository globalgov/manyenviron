# HUGGO Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many packages universe.

# Stage one: Collecting data
HUGGO <- readr::read_csv("data-raw/agreements/HUGGO/EnvGov Nodes-Table 1 VERS2.csv")
HUGGO2 <- readr::read_csv2("data-raw/agreements/HUGGO/endb4beg.csv")
# HUGGO3 <- readr::read_csv("data-raw/agreements/HUGGO/gnevar.csv")# Same as dataset 1
HUGGO4 <- readr::read_csv("data-raw/agreements/HUGGO/GENG v1.2 (31.10.2015).csv")
HUGGO5 <- readr::read_csv2("data-raw/agreements/HUGGO/duplicates v1.0.csv")
HUGGO6 <- readxl::read_excel("data-raw/agreements/HUGGO/multiGENG.xlsx", 
                     skip = 1)

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'HUGGO' object until the object created
# below (in stage three) passes all the tests.
HUGGO <- as_tibble(HUGGO)  %>%
  tidyr::separate(IEA, c("NEW", "IEADB_ID"), sep = "-") %>%
  dplyr::mutate(AgreementType = dplyr::recode(T, G = "A", M = "E", "T" = "Q",
                                              D = "V", R = "W", N = "X",
                                              U = "Y")) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DocSign),
                        End = messydates::as_messydate(DocEnd),
                        Force = messydates::as_messydate(DocForce),
                        gnevarID = GENG,
                        ecolexID = ECOLEX) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title,
                                                     api_key = api)) %>%
  # Define Key API
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::rename(DocType = L) %>%
  dplyr::rename(GeogArea = J) %>%
  dplyr::select(gnevarID, Title, Beg, End, DocType, GeogArea, AgreementType,
                Signature, Force) %>%
  dplyr::arrange(Beg)

# Add treatyID column
HUGGO$treatyID <- manypkgs::code_agreements(HUGGO,
                                            HUGGO$Title,
                                            HUGGO$Beg)

# # Clean HUGGO 2
HUGGO2 <- as_tibble(HUGGO2) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title,
                                                     api_key = api)) %>%
  # Define Key API
  dplyr::mutate(Beg = messydates::as_messydate(Beg)) %>%
  dplyr::mutate(End = messydates::as_messydate(End)) %>%
  dplyr::mutate(Force = messydates::as_messydate(Force)) %>%
  dplyr::mutate(Term = messydates::as_messydate(Term)) %>%
  manydata::transmutate(Signature = messydates::as_messydate(Sign))

# Add treatyID column
HUGGO2$treatyID <- manypkgs::code_agreements(HUGGO2, HUGGO2$Title, HUGGO2$Beg)

# Clean HUGGO4
HUGGO4$Parties <- paste0(HUGGO4$Country.x, "-", HUGGO4$Country.y)
HUGGO4 <- as_tibble(HUGGO4) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DocDate),
                     Force = messydates::as_messydate(InForce)) %>%
  dplyr::mutate(End = messydates::as_messydate(End)) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title,
                                                     api_key = api)) %>%
  # Define Key API
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::select(Title, Beg, Signature, Force, End, Parties)

# Add treatyID column
HUGGO4$treatyID <- manypkgs::code_agreements(HUGGO4,
                                              HUGGO4$Title,
                                              HUGGO4$Beg)

# Clean HUGGO5: the current ID format (MGENG-002)
# is not found in most other HUGGO datasets...

# Create a HUGGO "database" to apply consolidate()
HUGGO <- list(HUGGO, HUGGO2, HUGGO4)

# Join the datasets together
HUGGO <- manydata::consolidate(HUGGO, row = "any", cols = "any",
                               resolve = "coalesce", key = "treatyID")

# Add manyID column
manyID <- manypkgs::condense_agreements(manyenviron::agreements)
HUGGO <- dplyr::left_join(HUGGO, manyID, by = "treatyID")

# Extract texts
HUGGO_TXT <- manydata::consolidate(manyenviron::agreements, "any", "any",
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

# Step three: join IEADB text column with the consolidated version of
# manyenviron
HUGGO_TXT <- dplyr::left_join(HUGGO_TXT,
                              IEADB_original,
                              by = "ieadbID")
HUGGO_TXT <- as_tibble(HUGGO_TXT) %>%
  dplyr::select(treatyID, ieadbID, gnevarID, ecolexID,
                manyID, Title, Beg, TreatyText, Source)

# Step four: complement the dataset with ECOLEX treaty texts (TO BE IMPROVED)
# Filter observations with text = NULL and with an ecolexID
ecolex_text <- HUGGO_TXT %>%
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

ecolex_text <- ecolex_text %>%
  dplyr::filter(!is.na(Text)) %>%
  dplyr::select(ecolexID, Text)

ecolex_text$Source <- "ECOLEX"

# Step five: join ECOLEX texts to the agreements dataset
HUGGO_TXT <- dplyr::left_join(HUGGO_TXT,  ecolex_text, by = "ecolexID")
HUGGO_TXT$Source.x <- dplyr::na_if(HUGGO_TXT$Source.x, "NULL")
HUGGO_TXT$Source.y <- dplyr::na_if(HUGGO_TXT$Source.y, "NULL")

HUGGO_TXT$TreatyText <- dplyr::na_if(HUGGO_TXT$TreatyText, "NULL")
HUGGO_TXT$Text <- dplyr::na_if(HUGGO_TXT$Text, "NULL")
HUGGO_TXT$TreatyText <- dplyr::coalesce(HUGGO_TXT$TreatyText, HUGGO_TXT$Text)

# Step six: Clean texts
HUGGO_TXT <- HUGGO_TXT %>%
  dplyr::mutate(TreatyText = manypkgs::standardise_treaty_text(TreatyText))

HUGGO_TXT <- dplyr::as_tibble(HUGGO_TXT) %>%
  dplyr::rename(Source = `Source.x`) %>%
  dplyr::select(manyID, TreatyText, Source)

# Bind data
HUGGO <- dplyr::left_join(HUGGO, HUGGO_TXT)
# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# MEA nodes and Edges data (MGENG)
# For some nore information about the variables and codes,
# please see the documentation in the data-raw folder.
HUGGO6 <- as_tibble(HUGGO6) %>%
  manydata::transmutate(Signature = messydates::as_messydate(DocSign),
                        End = messydates::as_messydate(DocEnd),
                        Force = messydates::as_messydate(DocForce),
                        ecolexID = ECOLEX.ID) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(Beg)

HUGGO6$treatyID <- manypkgs::code_agreements(HUGGO6,
                                             HUGGO6$Title,
                                             HUGGO6$Beg)

# HUGGO6 data appears too take lots of memory?
a <- as.matrix(HUGGO6)
# Some columns are simply full of NAs


# Join the datasets together
HUGGO <- manydata::consolidate(HUGGO, HUGGO6, by = "treatyID")
# A full join here renders the data too big to work with in R...
manyID <- manypkgs::condense_agreements(var = HUGGO$treatyID)
HUGGO <- dplyr::left_join(HUGGO, manyID, by = c("treatyID", "manyID"))

# Stage three: Connecting data
# Next run the following line to make HUGGO available
# within the package.
manypkgs::export_data(HUGGO, database = "agreements", URL = "NA")
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
