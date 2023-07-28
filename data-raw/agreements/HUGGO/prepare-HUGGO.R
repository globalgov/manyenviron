# HUGGO Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many package.

# Stage one: Collecting data
HUGGO1 <- readr::read_csv("data-raw/agreements/HUGGO/EnvGov Nodes-Table 1 VERS2.csv")
HUGGO2 <- readr::read_csv("data-raw/agreements/HUGGO/gnevar.csv")
HUGGO4 <- readr::read_csv("data-raw/agreements/HUGGO/GENG v1.2 (31.10.2015).csv")
HUGGO6 <- readr::read_delim("data-raw/agreements/HUGGO/MEA.Nodes v1.0.csv",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'HUGGO' object until the object created
# below (in stage four) passes all the tests.
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

# Step three: join IEADB text column with the consolidated version of
# manyenviron
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
texts <- dplyr::left_join(texts,  ecolex_text, by = c("ecolexID", "Title",
                                                      "Beg","TreatyText",
                                                      "url"))

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

# Checked_HUGGO and Confirmed_HUGGO variables to
# track progress on manually correcting entries
# Checked_HUGGO: code 1 when the entire row's
# observations have been verified and updated
# Confirmed_HUGGO: list variables for which the
# observation could be verified and confirmed.
# Eg. List 'Signature' in `Confirmed_HUGGO` if
# the Signature date was found and verified in
# the treaty text or in a manual online search.
HUGGO$Checked_HUGGO <- NA
HUGGO$Confirmed_HUGGO <- NA

# Add Changes var to log changes that are manually added for each agreement.
HUGGO$Changes <- NA

# Stage three: Merging verified data and additional treaties into HUGGO dataset

# Step one: Merge data frames with verified metadata of treaties within and
# without HUGGO

# Load original HUGGO
HUGGO_or <- manyenviron::agreements$HUGGO

# Load data frame with verified metadata (of treaties present in HUGGO)
HUGGO7 <- read.csv("data-raw/agreements/HUGGO/HUGGO_verified.csv")

# Load data frame with verified metadata (of treaties not present in HUGGO)
HUGGO8 <- read.csv("data-raw/agreements/HUGGO/HUGGO_additional.csv")

# Create one data frame
HUGGO9 <- rbind(HUGGO7, HUGGO8)

# Drop text columns and the ones included for verification purposes
HUGGO9 <- HUGGO9 %>%
  dplyr::select(-c(Source, Checked_HUGGO, Confirmed_HUGGO, Changes, Modified))

# Original data from HUGGO
# Detect if any row has non-ASCII characters in manyID and
# replace it with the corresponding treatyID
HUGGO_or[which(stringr::str_detect(HUGGO_or$manyID, "[^[:ascii:]]")), 1] <-
  HUGGO_or[which(stringr::str_detect(HUGGO_or$manyID, "[^[:ascii:]]")), 13]

# Verified and additional data
# Detect if any row has non-ASCII characters in manyID and
# replace it with the corresponding treatyID
HUGGO9[which(stringr::str_detect(HUGGO9$manyID, "[^[:ascii:]]")), 1] <-
  HUGGO9[which(stringr::str_detect(HUGGO9$manyID, "[^[:ascii:]]")), 13]

# Merge data frames
HUGGO_new <- dplyr::full_join(HUGGO_or, HUGGO9,
                              by = c("manyID", "treatyID")) %>%
  dplyr::distinct() %>%
  dplyr::relocate(manyID, Title.x, Title.y, Beg.x, Beg.y, Signature.x,
                  Signature.y, Force.x, Force.y, End.x, End.y, Parties.x,
                  Parties.y)

# Clean merged data
HUGGO_new <- HUGGO_new %>%
  dplyr::mutate(Title = ifelse(!is.na(Title.y), Title.y, Title.x),
                Beg = ifelse(!is.na(Beg.y), Beg.y, Beg.x),
                Signature = ifelse(!is.na(Signature.y), Signature.y,
                                   Signature.x),
                End = ifelse(!is.na(End.y), End.y, End.x),
                Force = ifelse(!is.na(Force.y), Force.y, Force.x),
                url = ifelse(!is.na(url.y), url.y, url.x),
                Parties = ifelse(!is.na(Parties.y), Parties.y, Parties.x),
                AgreementType = ifelse(!is.na(AgreementType.y), AgreementType.y,
                                       AgreementType.x),
                DocType = ifelse(!is.na(DocType.y), DocType.y, DocType.x),
                GeogArea = ifelse(!is.na(GeogArea.y), GeogArea.y, GeogArea.x),
                ieaID = ifelse(!is.na(ieaID.y), ieaID.y, ieaID.x),
                gengID = ifelse(!is.na(gengID.y), gengID.y, gengID.x),
                ecolexID = ifelse(!is.na(ecolexID.y), ecolexID.y, ecolexID.x),
                verified = ifelse(!is.na(verified.y), verified.y, verified.x),
                DocValidUntilDate = ifelse(!is.na(DocValidUntilDate.y),
                                           DocValidUntilDate.y,
                                           DocValidUntilDate.x),
                Notes = ifelse(!is.na(Notes.y), Notes.y, Notes.x),
                Download = ifelse(!is.na(Download.y), Download.y, Download.x),
                MEA_type = ifelse(!is.na(MEA_type.y), MEA_type.y, MEA_type.x),
                Ambit = ifelse(!is.na(Ambit.y), Ambit.y, Ambit.x),
                Region = ifelse(!is.na(Region.y), Region.y, Region.x),
                subject_ecolex = ifelse(!is.na(subject_ecolex.y),
                                        subject_ecolex.y, subject_ecolex.x),
                subject_iea = ifelse(!is.na(subject_iea.y), subject_iea.y,
                                     subject_iea.x),
                Keywords = ifelse(!is.na(Keywords.y), Keywords.y, Keywords.x),
                Lineage = ifelse(!is.na(Lineage.y), Lineage.y, Lineage.x),
                Sequence = ifelse(!is.na(Sequence.y), Sequence.y, Sequence.x),
                AdoptedIn = ifelse(!is.na(AdoptedIn.y), AdoptedIn.y,
                                   AdoptedIn.x),
                Languages = ifelse(!is.na(Languages.y), Languages.y,
                                   Languages.x),
                Appendices = ifelse(!is.na(Appendices.y), Appendices.y,
                                    Appendices.x),
                Depository = ifelse(!is.na(Depository.y), Depository.y,
                                    Depository.x),
                DepositoryURL = ifelse(!is.na(DepositoryURL.y), DepositoryURL.y,
                                       DepositoryURL.x),
                Published = ifelse(!is.na(Published.y), Published.y,
                                   Published.x),
                Abstract = ifelse(!is.na(Abstract.y), Abstract.y, Abstract.x),
                Website1 = ifelse(!is.na(Website1.y), Website1.y, Website1.x),
                Website2 = ifelse(!is.na(Website2.y), Website2.y, Website2.y),
                Secretariat = ifelse(!is.na(Secretariat.y), Secretariat.y,
                                     Secretariat.x),
                SecretariatURL = ifelse(!is.na(SecretariatURL.y),
                                        SecretariatURL.y, SecretariatURL.x),
                UNEP = ifelse(!is.na(UNEP.y), UNEP.y, UNEP.x),
                Supersedes = ifelse(!is.na(Supersedes.y), Supersedes.y,
                                    Supersedes.x),
                References = ifelse(!is.na(References.y), References.y,
                                    References.x),
                EnabledBy = ifelse(!is.na(EnabledBy.y), EnabledBy.y,
                                   EnabledBy.x),
                AmendedBy = ifelse(!is.na(AmendedBy.y), AmendedBy.y,
                                   AmendedBy.x),
                Lit = ifelse(!is.na(Lit.y), Lit.y, Lit.x),
                Data = ifelse(!is.na(Data.y), Data.y, Data.x),
                Coded = ifelse(!is.na(Coded.y), Coded.y, Coded.x))%>%
  dplyr::distinct() %>%
  dplyr::select(-c(Title.x, Title.y, Beg.x, Beg.y, End.x, End.y, Signature.x,
                   Signature.y, Force.x, Force.y, Abstract.x, Abstract.y,
                   Parties.x, Parties.y, AgreementType.x, AgreementType.y,
                   DocType.y, DocType.x, GeogArea.x, GeogArea.y, gengID.x,
                   gengID.y, ieaID.x, ieaID.y, ecolexID.x, ecolexID.y,
                   verified.x, verified.y, DocValidUntilDate.x,
                   DocValidUntilDate.y, url.x, url.y, Notes.x, Notes.y,
                   Download.x, Download.y, MEA_type.x, MEA_type.y, Ambit.x,
                   Ambit.y, Region.x, Region.y, subject_ecolex.x,
                   subject_ecolex.y, subject_iea.x, subject_iea.y,
                   Keywords.x, Keywords.y, Lineage.x, Lineage.y, Sequence.x,
                   Sequence.y, AdoptedIn.x, AdoptedIn.y, Languages.x,
                   Languages.y, Appendices.x, Appendices.y, Depository.x,
                   Depository.y, DepositoryURL.x, DepositoryURL.y, Published.x,
                   Published.y, Website1.x, Website1.y, Website2.x, Website2.y,
                   Secretariat.x, Secretariat.y, SecretariatURL.x,
                   SecretariatURL.y, UNEP.x, UNEP.y,Supersedes.x, Supersedes.y,
                   References.x, References.y, EnabledBy.x, EnabledBy.y,
                   AmendedBy.x, AmendedBy.y, Lit.x, Lit.y, Data.x, Data.y,
                   Coded.x, Coded.y)) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title)) %>%
  dplyr::relocate(manyID, Title, Beg, End, Signature, Force, url,
                  AgreementType, DocType, GeogArea, gengID, ieaID, ecolexID,
                  treatyID, Parties, verified, DocValidUntilDate, Notes,
                  Download, MEA_type, Ambit, Region, subject_ecolex,
                  subject_iea, Keywords, Lineage, Sequence, AdoptedIn,
                  Languages, Appendices, Depository, DepositoryURL,
                  Published, Website1, Website2, Secretariat, SecretariatURL,
                  UNEP, Supersedes, References, EnabledBy, AmendedBy, Lit,
                  Data, Coded, Abstract) %>%
  dplyr::arrange(Beg)

# Step two: Clean duplicate rows

# First row NA remove
HUGGO_new <- HUGGO_new[-(which(is.na(HUGGO_new$manyID))), ]

# Clean rows that share the same manyID

# BAD-CHE[LCT]_1884P
# Additional Convention Between Switzerland Baden And Alsace-Lorraine
# Concerning Fishing In Lake Constance And Its Tributaries
which(HUGGO_new$manyID == "BAD-CHE[LCT]_1884P")
remove <- which(HUGGO_new$manyID == "BAD-CHE[LCT]_1884P" &
                  HUGGO_new$gengID == "GENG-0077")
# Keep one row
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# Protocol To The Additional Convention Between Switzerland Baden And
# Alsace-Lorraine Concerning Fishing In Lake Constance And Its Tributaries
which(HUGGO_new$manyID == "BAD-CHE[LCT]_1884P")
remove <- which(HUGGO_new$manyID == "BAD-CHE[LCT]_1884P" &
                  HUGGO_new$gengID == "GENG-0078")
# Keep one row
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# CAN-USA[LLW]_1925P:CAN-USA[LLW]_1925A
which(HUGGO_new$manyID == "CAN-USA[LLW]_1925P:CAN-USA[LLW]_1925A")
# Keep row with verified information (Force)
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "CAN-USA[LLW]_1925P:CAN-USA[LLW]_1925A"
                               & HUGGO_new$Force == "1925-07-17")), ]

# OP06SP_1952A
which(HUGGO_new$manyID == "OP06SP_1952A")
# Keep row with more information
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "OP06SP_1952A"
                               & !is.na(HUGGO_new$Region))), ]

# CDSMMZ_1954P20
which(HUGGO_new$manyID == "CDSMMZ_1954P20")
# Fully duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "CDSMMZ_1954P20")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# ESP-PRT[FSF]_1969A
which(HUGGO_new$manyID == "ESP-PRT[FSF]_1969A")
# Fully duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "ESP-PRT[FSF]_1969A")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# ELCCDF_1972A
which(HUGGO_new$manyID == "ELCCDF_1972A")
# Duplicate rows, keep the one considered when verifying metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ELCCDF_1972A"
                               & is.na(HUGGO_new$url))), ]

# RUS-USA[HSA]_1975A
which(HUGGO_new$manyID == "RUS-USA[HSA]_1975A")
# Two fully duplicated treaties, remove duplicated rows
remove <- which(HUGGO_new$manyID == "RUS-USA[HSA]_1975A"
                & HUGGO_new$gengID == "GENG-1181")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "RUS-USA[HSA]_1975A"
                & HUGGO_new$gengID == "GENG-1248")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# CAN-NOR[CSS]_1975N
which(HUGGO_new$manyID == "CAN-NOR[CSS]_1975N")
# Two fully duplicated treaties, remove duplicated rows
remove <- which(HUGGO_new$manyID == "CAN-NOR[CSS]_1975N"
                & HUGGO_new$Force == "1975-04-23")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "CAN-NOR[CSS]_1975N"
                & HUGGO_new$Force == "1975-12-12")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# GU14US_1975A
which(HUGGO_new$manyID == "GU14US_1975A")
# Two fully duplicated treaties, remove duplicated rows
remove <- which(HUGGO_new$manyID == "GU14US_1975A"
                & HUGGO_new$Signature == "1975-05-30")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "GU14US_1975A"
                & HUGGO_new$Signature == "1975-12-16")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# GUY-RUS[UNF]_1977A
which(HUGGO_new$manyID == "GUY-RUS[UNF]_1977A")
# Fully duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "GUY-RUS[UNF]_1977A")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# DEU-LUX[FFA]_1980N
which(HUGGO_new$manyID == "DEU-LUX[FFA]_1980N")
# Duplicate rows, keep one with more data
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "DEU-LUX[FFA]_1980N"
                               & is.na(HUGGO_new$Ambit))), ]

# ESPOTF_1983A
which(HUGGO_new$manyID == "ESPOTF_1983A")
# Full duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "ESPOTF_1983A")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# CAN-USA[FCS]_1989A
which(HUGGO_new$manyID == "CAN-USA[FCS]_1989A")
# Full duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "CAN-USA[FCS]_1989A")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# AUS-JPN[LLF]_1990P
which(HUGGO_new$manyID == "AUS-JPN[LLF]_1990P")
# Full duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "AUS-JPN[LLF]_1990P")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# TPNWLA_1991E:TPNWLA_1967A
which(HUGGO_new$manyID == "TPNWLA_1991E:TPNWLA_1967A")
# Duplicate rows, keep one with more information
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "TPNWLA_1991E:TPNWLA_1967A"
                               & is.na(HUGGO_new$url))), ]

# SWZ-ZAF[DUK]_1992A
which(HUGGO_new$manyID == "SWZ-ZAF[DUK]_1992A")
# Fully duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "SWZ-ZAF[DUK]_1992A")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# RUS-UKR[FSA]_1993A
which(HUGGO_new$manyID == "RUS-UKR[FSA]_1993A")
# Duplicate rows, keep the one with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "RUS-UKR[FSA]_1993A"
                               & is.na(HUGGO_new$url))), ]

# EB06DB_1993A
which(HUGGO_new$manyID == "EB06DB_1993A")
# Fully duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "EB06DB_1993A")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# BJ06TP_1995A
# Two fully duplicated treaties
# Jordan and Sudan
remove <- which(HUGGO_new$manyID == "BJ06TP_1995A"
                & HUGGO_new$Beg == "1995-02-19")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
# Jordan and Tunis
remove <- which(HUGGO_new$manyID == "BJ06TP_1995A"
                & HUGGO_new$Beg == "1995-04-27")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# CTMHWD_1995E:CTMHWD_1989A
which(HUGGO_new$manyID == "CTMHWD_1995E:CTMHWD_1989A")
# Duplicate rows, keep one with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "CTMHWD_1995E:CTMHWD_1989A"
                               & is.na(HUGGO_new$url))), ]

# GIN-EC[ECF]_1995P
which(HUGGO_new$manyID == "GIN-EC[ECF]_1995P")
# Fully duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "GIN-EC[ECF]_1995P")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# CE06NS_1999A
which(HUGGO_new$manyID == "CE06NS_1999A")
# Two fully duplicated treaties
# Remove duplicate rows
remove <- which(HUGGO_new$manyID == "CE06NS_1999A"
                & HUGGO_new$Beg == "1999-07-19")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "CE06NS_1999A"
                & HUGGO_new$Beg == "1999-07-23")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# AGO-EC[CPF]_2000P
which(HUGGO_new$manyID == "AGO-EC[CPF]_2000P")
# Two fully duplicated treaties, remove duplicate rows
remove <- which(HUGGO_new$manyID == "AGO-EC[CPF]_2000P"
                & HUGGO_new$Beg == "2000-03-30")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "AGO-EC[CPF]_2000P"
                & HUGGO_new$Beg == "2000-07-06")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# SEN-EC[CFP]_2002A
which(HUGGO_new$manyID == "SEN-EC[CFP]_2002A")
# Two fully duplicated treaties, remove duplicate rows
remove <- which(HUGGO_new$manyID == "SEN-EC[CFP]_2002A"
                & HUGGO_new$End == "2001-12-31")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "SEN-EC[CFP]_2002A"
                & HUGGO_new$End == "2001-07-31")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# CRI-USA[TTE]_2003A
which(HUGGO_new$manyID == "CRI-USA[TTE]_2003A")
# Fully duplicated treaty, remove duplicate row
remove <- which(HUGGO_new$manyID == "CRI-USA[TTE]_2003A")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# DNK-EC[RGH]_2007A1:DNK-EC[RGH]_2007A1
which(HUGGO_new$manyID == "DNK-EC[RGH]_2007A1:DNK-EC[RGH]_2007A1")
# Fully duplicated treaty, remove duplicate rows
remove <- which(HUGGO_new$manyID == "DNK-EC[RGH]_2007A1:DNK-EC[RGH]_2007A1")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# ID04MN_2004E8
which(HUGGO_new$manyID == "ID04MN_2004E8")
# Two fully duplicated treaties, remove duplicate rows
remove <- which(HUGGO_new$manyID == "ID04MN_2004E8" & is.na(HUGGO_new$url))
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "ID04MN_2004E8" & !is.na(HUGGO_new$url))
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# Clean rows that have data about the same agreement, but with
# different manyID

# Convention Between Finland And Russia With Regard To Fishing And Sealing On
# Lake Ladoga Helsingfors
id <- "GENG-0218"
which(HUGGO_new$gengID == gengID)
## Keep row with verified data, but match manyID
HUGGO_new[which(HUGGO_new$gengID == id & !is.na(HUGGO_new$url)), 1] <-
  HUGGO_new[which(HUGGO_new$gengID == id & is.na(HUGGO_new$url)), 1]
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$gengID == id &
                                 is.na(HUGGO_new$url))), ]

# Convention Between The Union Of Soviet Socialist Republics And Finland
# Concerning The Maintenance Of River Channels And The Regulation Of Fishing On
# Watercourses Forming Part Of The Frontier Between Russia And Finland
id <- "GENG-0219"
which(HUGGO_new$gengID == id)
## Keep row with verified data, but match manyID
HUGGO_new[which(HUGGO_new$gengID == id & !is.na(HUGGO_new$url)), 1] <-
  HUGGO_new[which(HUGGO_new$gengID == id & is.na(HUGGO_new$url)), 1]
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$gengID == id &
                                 is.na(HUGGO_new$url))), ]

# Protocol For The Prohibition Of The Use In War Of Asphyxiating Poisonous Or
# Other Gases And Of Bacteriological Methods Of Warfare
id <- "GENG-0240"
which(HUGGO_new$gengID == id)
# Keep row with verified data and manyID matching with treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "PU04MW_1925P"
                               & HUGGO_new$gengID == id)), ]

# Nordic Mutual Emergency Assistance Agreement In Connection With Radiation
# Accidents
id <- "GENG-0679"
which(HUGGO_new$gengID == id)
# Duplicated row under different manyID.
# Keep row with manyID matching treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$gengID == id
                               & HUGGO_new$manyID == "AR04SN_1963A")), ]

# Agreement Between The Government Of The United States Of America And The
# Government Of The Union Of Soviet Socialist Republics Relating To Fishing For
# King And Tanner Crab
id <- "RUS-USA[KTC]_1973A"
which(HUGGO_new$treatyID == id)
## Keep row with verified data and manyID matching with treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == id
                               & HUGGO_new$manyID == "RUS-USA[CSC]_1973A")), ]

# Agreement Between The Government Of The United States Of America And The
# Government Of The Union Of Soviet Socialist Republics Relating To Fishing
# Operations In The North Eastern Pacific Ocean
id <- "RUS-USA[UFO]_1973A"
which(HUGGO_new$treatyID == id)
# Keep row with verified data, but match manyID to treatyID
HUGGO_new[which(!is.na(HUGGO_new$url) &
                  HUGGO_new$treatyID == id), 1] <-
  HUGGO_new[which(is.na(HUGGO_new$url) & HUGGO_new$treatyID == id), 1]
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == id &
                                 is.na(HUGGO_new$url))), ]

# Agreement Between The Government Of The United States Of America And The
# Government Of The USSR Relating To The Consideration Of Claims Resulting From
# Damage To Fishing Vessels Or Gear And Measures To Prevent Fishing Conflicts
id <- "RUS-USA[PFC]_1973A"
which(HUGGO_new$treatyID == "RUS-USA[PFC]_1973A")
# Keep row with verified data
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == "RUS-USA[PFC]_1973A" &
                                 HUGGO_new$manyID == "RUS-USA[CSC]_1973A")), ]

# Agreement Between The Government Of The United States Of America And The
# Government Of The Union Of Soviet Socialist Republics On Certain Fishery
# Problems On The High Seas In The Western Areas Of The Middle Atlantic Ocean
id <- "RUS-USA[HSA]_1973A"
which(HUGGO_new$treatyID == id)
# Keep row with verified data
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == id &
                                 HUGGO_new$manyID == "RUS-USA[CSC]_1973A")), ]

# Agreement Between The Government Of The Union Of Soviet Socialist Republics
# And The Government Of The Peoples Republic Of Bulgaria Concerning Fishing For
# Anchovies And Sprats In Each Others Territorial Waters In The Black Sea
id <- "BGR-RUS[WBS]_1978A"
which(HUGGO_new$treatyID == id)
## Keep row with verified data
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == id &
                                 is.na(HUGGO_new$url))), ]

# Agreement Between The Cabinet Of Ministers Of Belarus And The Government Of
# The Russian Federation On Cooperation In Fuel And Energy Sector
id <- "BLR-RUS[FES]_1994A"
which(HUGGO_new$treatyID == id)
# Keep row with verified data
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == id &
                                 is.na(HUGGO_new$url))), ]

# Convention On Conservation And Development Of Fishery Resources In The Border
# Sections Of The Parana And Paraguay Rivers Between Argentina And Paraguay
id <- "ARG-PRY[SPR]_1996A"
which(HUGGO_new$treatyID == id)
## Keep row with verified data
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == id &
                                 is.na(HUGGO_new$url))), ]

# Agreement Between Romania And Hungary Regarding Rapid Notification Of A
# Nuclear Accident
id <- "HUN-ROU[NNA]_1997A"
which(HUGGO_new$treatyID == id)
## Keep row with verified data, but match manyID
HUGGO_new[which(HUGGO_new$treatyID == id & !is.na(HUGGO_new$url)), 1] <-
  HUGGO_new[which(HUGGO_new$treatyID == id & is.na(HUGGO_new$url)), 1]
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == id &
                                 is.na(HUGGO_new$url))), ]

# Agreement Between The Government Of The
# Republic Of Lithuania And The Government
# Of The Russian Federation On The Cooperation
# In The Field Of Fishing
id <- "LTU-RUS[RFF]_1999A"
which(HUGGO_new$treatyID == id)
## Keep row with verified data, but match manyID
HUGGO_new[which(HUGGO_new$treatyID == id & !is.na(HUGGO_new$url)), 1] <-
  HUGGO_new[which(HUGGO_new$treatyID == id & is.na(HUGGO_new$url)), 1]
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == id &
                                 is.na(HUGGO_new$url))), ]

# Agreement Between The State Committee Of Energy Saving Of Ukraine And The
# Committee Of Energetic Efficiency Of Belarus On Cooperation In The Sphere Of
# Energetic Efficiency And Renewable Energy
id <- "BLR-UKR[ERE]_2001A"
which(HUGGO_new$treatyID == id)
# Keep row with verified data
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == id &
                                 is.na(HUGGO_new$url))), ]

# Additional Protocol To The Environment Treaty Between The Government Of
# Argentina And The Government Of The Republic Of Bolivia
id <- "ARG-BOL[ENB]_2004P"
which(HUGGO_new$treatyID == id)
## Keep row with verified data, but match manyID
HUGGO_new[which(HUGGO_new$treatyID == id & !is.na(HUGGO_new$url)), 1] <-
  HUGGO_new[which(HUGGO_new$treatyID == id & is.na(HUGGO_new$url)), 1]
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == id &
                                 is.na(HUGGO_new$url))), ]

# Cooperation Agreement Between The Government Of The Republic Of Paraguay And
# The Government Of The Federative Republic Of Brazil For Sustainable
# Development And Integrated Management Of The Apa River Basin
id <- "BRA-PRY[IMA]_2006A"
which(HUGGO_new$treatyID == "BRA-PRY[IMA]_2006A")
# Keep row with verified data
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == "BRA-PRY[IMA]_2006A" &
                                 (is.na(HUGGO_new$url)))), ]

# Agreement Between The Government Of The Russian Federation And The Government
# Of South Africa In The Sphere Of Water Relations And Forest Management
id <- "RUS-ZAF[RFM]_2007A"
which(HUGGO_new$treatyID == id)
# Keep row with verified data, but match manyID
HUGGO_new[which(HUGGO_new$treatyID == id & !is.na(HUGGO_new$url)), 1] <-
  HUGGO_new[which(HUGGO_new$treatyID == id & is.na(HUGGO_new$url)), 1]
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$treatyID == id &
                                 is.na(HUGGO_new$url))), ]

# Agreement Between The Cabinet Of Ministers Of Ukraine And The Government Of
# Belarus On Joint Management And Protection Of Transboundary Waterbodies
title <- "Protection Of Transboundary Waterbodies"
parties <- "BLR-UKR"
which(grepl(title, HUGGO_new$Title) & HUGGO_new$Parties == parties)
# Two duplicate rows under different manyID. Remove row with less info,
# but keep url and Force
HUGGO_new[which(grepl(title, HUGGO_new$Title) & HUGGO_new$Parties == parties &
                  is.na(HUGGO_new$url)), 7] <-
  HUGGO_new[which(grepl(title, HUGGO_new$Title) & HUGGO_new$Parties == parties &
                    !is.na(HUGGO_new$url)), 7]
HUGGO_new[which(grepl(title, HUGGO_new$Title) & HUGGO_new$Parties == parties &
                  is.na(HUGGO_new$url)), 6] <-
  HUGGO_new[which(grepl(title, HUGGO_new$Title) & HUGGO_new$Parties == parties &
                    !is.na(HUGGO_new$AgreementType)), 6]
HUGGO_new <- HUGGO_new[-(which(grepl(title, HUGGO_new$Title) &
                                 HUGGO_new$Parties == parties &
                                 is.na(HUGGO_new$AgreementType) &
                                 HUGGO_new$Beg == "2001-10-16")), ]

# Step three: TreatyText and Language columns

# Add columns to show if the raw text of the agreements has been collected and
# to indicate its language

HUGGO_new <- HUGGO_new %>%
  dplyr::mutate(TreatyText = NA) %>%
  dplyr::mutate(Language = NA)

# Step four: Standardise date columns, arrange by Beg,
# and push HUGGO_new to HUGGO
HUGGO <- HUGGO_new %>%
  dplyr::mutate(Beg = messydates::as_messydate(Beg),
                Signature = messydates::as_messydate(Signature),
                Force = messydates::as_messydate(Force),
                End = messydates::as_messydate(End)) %>%
  dplyr::arrange(Beg)

# Stage four: Matching agreements with titles in different languages
noneng <- HUGGO %>%
  # subset agreements with non-English titles but match a row with an English title
  dplyr::filter(manyID == "UB08IB_1893A" | manyID == "ESP-FRA[DGP]_1900A" |
                  manyID == "ED19DL_1905A" | manyID == "CHE-FRA[DDK]_1930A" |
                  manyID == "ARG-PRY[DRP]_1939A" | manyID == "CP09RP_1971A" |
                  manyID == "CP09RP_1971A" | manyID == "TRTDDY_1973A" |
                  manyID == "AS13DH_1978A" | manyID == "AT24JD_1979A" |
                  manyID == "AD25ED_1979A" | manyID == "MAR-ROU[SDR]_1979A" |
                  manyID == "BEL-LUX[EDL]_1980A" | manyID == "COL-ECU[SPA]_1994A" |
                  manyID == "FRA-NOR[PLM]_1995A" | manyID == "AS32AS_1995A" |
                  manyID == "AM17LS_1996A" | manyID == "ARG-PRY[LPN]_1996A" |
                  manyID == "AE21LM_1997A" | manyID == "AM17LD_1997A" |
                  manyID == "ARG-BOL[RDB]_1998A" | manyID == "TD13DP_1998A" |
                  manyID == "AP17RT_2002A" | manyID == "CHN-TUN[DTM]_2002A" |
                  manyID == "DZA-TUR[RDT]_2002A" | manyID == "AM23AE_2002A" |
                  manyID == "AE38LD_2002A" | manyID == "AE19DL_2003A" |
                  manyID == "AE24LJ_2003A"| manyID == "EL50LJ_2004A" |
                  manyID == "ARG-BOL[RDB]_2004P" | manyID == "AB22LA_2005A" |
                  manyID == "HND-MEX[EUM]_2005A" | manyID == "CHL-DZA[RDC]_2005A" |
                  manyID == "DZA-PER[RDP]_2005A" | manyID == "AC46LJ_2005A" |
                  manyID == "CD34RM_2005N1" | manyID == "MD19CC_2005S" |
                  manyID == "AM15DC_2006A" | manyID == "AC43LO_2006A" |
                  manyID == "NIC-VEN[LRD]_2007A" | manyID == "AC33LJ_2007A"|
                  manyID == "ARG-ECU[LHE]_2007A" | manyID == "AC11PB_2007A" |
                  manyID == "AD33LN_2007A" | manyID == "AC34LD_2007A" |
                  manyID == "AD25LF_2008A" | manyID == "BRA-CRI[DSL]_2008A"|
                  manyID == "BRA-CRI[PDB]_2008A" | manyID == "BRA-CRI[DCE]_2008P" |
                  manyID == "AE22LE_2008A" | manyID == "AS35LO_2008A" |
                  manyID == "CP09LA_2012O"| manyID == "CC09DM_1954A" |
                  manyID == "VB13GV_2008O" | manyID == "CS18PS_1954A"|
                  manyID == "CS18PS_1954A")
noneng <- noneng %>%
  dplyr::rename(match = manyID, Orig_noneng_title = Title) %>%
  dplyr::arrange(match) %>%
  # add in matching list of manyIDs of agreements with English titles
  dplyr::mutate(manyID = c("FRA-URY[CCM]_2005A", "SI04DB_2007P", "FRA-USA[PPP]_2007A",
                           "UKR-ALG[PPA]_2007A", "FRA-TUN[CCT]_2006P:UNFCCC_1992A",
                           "BRA-FRA[MKP]_2005P", "ESP-SEN[FAD]_1979A", "FRA-ZAF[ECT]_2008A",
                           "CHN-FRA[NEB]_2007A", "FRA-GAB[HWD]_2003A", "FRA-TUR[FEP]_1997A",
                           "VEN-ZAF[ENR]_2008A", "GF13JT_2003A", "FRA-KIR[EZT]_2002A",
                           "COG-ALG[MRD]_2006A", "FRA-LVA[MFR]_1997A", "FRA-TUR[MFP]_1996A",
                           "ARG-ALG[DSI]_2002A", "TUN-ALG[MBD]_2002A", "ARG-BOL[EIB]_1998A",
                           "ARG-BOL[ENB]_2004P", "ARG-ECU[SHE]_2007A", "ARG-PRY[LRP]_1939A",
                           "ARG-PRY[PNP]_1996A", "COL-HTI[MSA]_1978A", "CHE-FRA[LGP]_1995A",
                           "DEU-FRA[SFP]_2008A",
                           "ESP-MAR[MFR]_1979A","KB04WS_1980A","BRA-CRI[MCT]_2008P",
                           "BRA-CRI[ASL]_2008P","BRA-CRI[ABP]_2008P","CDSMZM_1954P20",
                           "ESP-PRT[ISM]_2005N","CHE-FRA[BDK]_1930A","CHL-ALG[EMD]_2005A",
                           "CHN-TUN[PMT]_2002A","COL-ECU[ARF]_1994A","ESTANA_2012A",
                           "STDYPR_1971A", "INVEPR_1971A", "ES06SP_1954A","PER-ALG[EMD]_2005A",
                           "TUR-ALG[END]_2002A","CF08CL_1905N11","FRA-MCO[TMS]_2004A",
                           "FS10PJ_1900A","FRA-NOR[SSP]_1995A","HND-MEX[MDM]_2005A",
                           "MAR-ROU[TMS]_1979A","ESP-GTM[ECC]_2005S","NIC-VEN[ENS]_2007A",
                           "ECU-PER[CMG]_1998A","YCYRTT_1973A","AH12IF_1893A", "TTICPO_2008A"
  ))
noneng <- dplyr::select(noneng, manyID, Orig_noneng_title, Beg, match)
# Non-English titles of the same agreement with an English title added in variable 'Orig_noneng_title'
HUGGO <- dplyr::left_join(HUGGO, noneng, by = c("manyID", "Beg"))
# remove rows with non-English titles identified above
HUGGO <- HUGGO[-which(HUGGO$manyID == "UB08IB_1893A" | HUGGO$manyID == "ESP-FRA[DGP]_1900A" |
                         HUGGO$manyID == "ED19DL_1905A" | HUGGO$manyID == "CHE-FRA[DDK]_1930A" |
                         HUGGO$manyID == "ARG-PRY[DRP]_1939A" | HUGGO$manyID == "CP09RP_1971A" |
                         HUGGO$manyID == "CP09RP_1971A" | HUGGO$manyID == "TRTDDY_1973A" |
                         HUGGO$manyID == "AS13DH_1978A" | HUGGO$manyID == "AT24JD_1979A" |
                         HUGGO$manyID == "AD25ED_1979A" | HUGGO$manyID == "MAR-ROU[SDR]_1979A" |
                         HUGGO$manyID == "BEL-LUX[EDL]_1980A" | HUGGO$manyID == "COL-ECU[SPA]_1994A" |
                         HUGGO$manyID == "FRA-NOR[PLM]_1995A" | HUGGO$manyID == "AS32AS_1995A" |
                         HUGGO$manyID == "AM17LS_1996A" | HUGGO$manyID == "ARG-PRY[LPN]_1996A" |
                         HUGGO$manyID == "AE21LM_1997A" | HUGGO$manyID == "AM17LD_1997A" |
                         HUGGO$manyID == "ARG-BOL[RDB]_1998A" | HUGGO$manyID == "TD13DP_1998A" |
                         HUGGO$manyID == "AP17RT_2002A" | HUGGO$manyID == "CHN-TUN[DTM]_2002A" |
                         HUGGO$manyID == "DZA-TUR[RDT]_2002A" | HUGGO$manyID == "AM23AE_2002A" |
                         HUGGO$manyID == "AE38LD_2002A" | HUGGO$manyID == "AE19DL_2003A" |
                         HUGGO$manyID == "AE24LJ_2003A"| HUGGO$manyID == "EL50LJ_2004A" |
                         HUGGO$manyID == "ARG-BOL[RDB]_2004P" | HUGGO$manyID == "AB22LA_2005A" |
                         HUGGO$manyID == "HND-MEX[EUM]_2005A" | HUGGO$manyID == "CHL-DZA[RDC]_2005A" |
                         HUGGO$manyID == "DZA-PER[RDP]_2005A" | HUGGO$manyID == "AC46LJ_2005A" |
                         HUGGO$manyID == "CD34RM_2005N1" | HUGGO$manyID == "MD19CC_2005S" |
                         HUGGO$manyID == "AM15DC_2006A" | HUGGO$manyID == "AC43LO_2006A" |
                         HUGGO$manyID == "NIC-VEN[LRD]_2007A" | HUGGO$manyID == "AC33LJ_2007A"|
                         HUGGO$manyID == "ARG-ECU[LHE]_2007A" | HUGGO$manyID == "AC11PB_2007A" |
                         HUGGO$manyID == "AD33LN_2007A" | HUGGO$manyID == "AC34LD_2007A" |
                         HUGGO$manyID == "AD25LF_2008A" | HUGGO$manyID == "BRA-CRI[DSL]_2008A"|
                         HUGGO$manyID == "BRA-CRI[PDB]_2008A" | HUGGO$manyID == "BRA-CRI[DCE]_2008P" |
                         HUGGO$manyID == "AE22LE_2008A" | HUGGO$manyID == "AS35LO_2008A" |
                         HUGGO$manyID == "CP09LA_2012O"| HUGGO$manyID == "CC09DM_1954A" |
                         HUGGO$manyID == "VB13GV_2008O" | HUGGO$manyID == "CS18PS_1954A"|
                         HUGGO$manyID == "CS18PS_1954A"),]

## Stage five: update End variable
## Recoded 9999-12-31 for treaties still in force
HUGGO_ver <- read.csv("data-raw/agreements/HUGGO/HUGGO_verified.csv")
for (i in 1:nrow(HUGGO)){
  title <- as.character(HUGGO[i, "Title"])
  manyID <- as.character(HUGGO[i, "manyID"])
  beg <- as.character(HUGGO[i, "Beg"])
  End_verified <- HUGGO_ver[which(HUGGO_ver$manyID == manyID &
                                    HUGGO_ver$Title == title &
                                    HUGGO_ver$Begin == beg), "End"]
  Force_verified <- HUGGO_ver[which(HUGGO_ver$manyID == manyID &
                                      HUGGO_ver$Title == title &
                                      HUGGO_ver$Begin == beg), "Force"]
  if (length(End_verified) > 0){
    HUGGO[i, "End"] <- messydates::as_messydate(End_verified)
  }
  if (length(Force_verified) > 0){
    HUGGO[i, "Force"] <- messydates::as_messydate(Force_verified)
  }
}

HUGGO_add <- read.csv("data-raw/agreements/HUGGO/HUGGO_additional.csv")
for (i in 1:nrow(HUGGO)){
  title <- as.character(HUGGO[i, "Title"])
  manyID <- as.character(HUGGO[i, "manyID"])
  beg <- as.character(HUGGO[i, "Beg"])
  End_add <- HUGGO_add[which(HUGGO_add$manyID == manyID &
                               HUGGO_add$Title == title &
                               HUGGO_add$Begin == beg), "End"]
  Force_add<- HUGGO_ver[which(HUGGO_add$manyID == manyID &
                                HUGGO_add$Title == title &
                                HUGGO_add$Begin == beg), "Force"]
  if (length(End_add) > 0){
    HUGGO[i, "End"] <- messydates::as_messydate(End_add)
  }
  if (length(Force_add) > 0){
    HUGGO[i, "Force"] <- messydates::as_messydate(Force_add)
  }
}

## Stage six: rename Beg column
HUGGO <- HUGGO %>%
  dplyr::rename("Begin" = "Beg")

# Stage six: Connecting data
# Next run the following line to make HUGGO available
# within the package.
manypkgs::export_data(HUGGO, datacube = "agreements",
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

