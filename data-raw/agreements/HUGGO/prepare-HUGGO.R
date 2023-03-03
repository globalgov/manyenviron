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
HUGGO1$treatyID <- manypkgs::code_agreements(HUGGO1,
                                             HUGGO1$Title,
                                             HUGGO1$Beg)

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
HUGGO2$treatyID <- manypkgs::code_agreements(HUGGO2, HUGGO2$Title, HUGGO2$Beg)

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
HUGGO4$treatyID <- manypkgs::code_agreements(HUGGO4,
                                             HUGGO4$Title,
                                             HUGGO4$Beg)

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
HUGGO6$treatyID <- manypkgs::code_agreements(HUGGO6, HUGGO6$Title, HUGGO6$Beg)

# Join the datasets together
## fixed minor coding issue with HUGGO before joining data (28-09-2022)
# HUGGO$Source <- as.character(HUGGO$Source)

# Create a HUGGO "database" to apply consolidate()
HUGGO <- list(HUGGO1, HUGGO2, HUGGO4, HUGGO6)

# Join the datasets together
HUGGO <- manydata::consolidate(HUGGO, row = "any", cols = "any",
                               resolve = "coalesce", key = "treatyID")

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

## Stage three: merge verified data and additional treaties into HUGGO dataset

## Load original HUGGO
HUGGO_ori <- manyenviron::agreements$HUGGO

## Load data frame with verified metadata (of treaties present in HUGGO)
HUGGO7 <- read.csv("data-raw/agreements/HUGGO/HUGGO_verified.csv", sep = ";", 
                   na.strings = "#N/D")

## Load data frame with verified metadata (of treaties not present in HUGGO)
HUGGO8 <- read.csv("data-raw/agreements/HUGGO/HUGGO_additional.csv", sep = ";", 
                   na.strings = "#N/D")

## Same row columns
colnames(HUGGO7) <- colnames(HUGGO8)

## Crate one data frame with both 
HUGGO9 <- rbind(HUGGO7, HUGGO8)

## Drop text columns and the ones included for verification purposes
HUGGO9 <- HUGGO9[, -c(47:51)]

## Add column for treaty text collection
columnnames <- append(colnames(HUGGO_ori), "TreatyText")
HUGGO9[, 47] <- 1
HUGGO_ori[, 47] <- NA

## Assign same column names to both dataframes
colnames(HUGGO9) <- columnnames
colnames(HUGGO_ori) <- columnnames

## Merge data frames
HUGGO_new <- dplyr::full_join(HUGGO_ori, HUGGO9, by = c("manyID", "treatyID")) %>%
  dplyr::distinct() %>%
  dplyr::relocate(manyID, Title.x, Title.y, Beg.x, Beg.y, Signature.x, 
                  Signature.y, Force.x, Force.y, End.x, End.y, Parties.x, 
                  Parties.y)

# Clean merged data
HUGGO_new <- HUGGO_new %>%
  dplyr::mutate(Title = ifelse(!is.na(Title.y), Title.y, Title.x),
                Beg = ifelse(!is.na(Beg.y), Beg.y, Beg.x), 
                Signature = ifelse(!is.na(Signature.y), Signature.y, Signature.x),  
                End = ifelse(!is.na(End.y), End.y, End.x),
                Force = ifelse(!is.na(Force.y), Force.y, Force.x), 
                url = ifelse(!is.na(url.y), url.y, url.x), 
                Parties = ifelse(!is.na(Parties.y), Parties.y, Parties.x),  
                TreatyText = ifelse(!is.na(TreatyText.y), TreatyText.y, TreatyText.x),
                AgreementType = ifelse(!is.na(AgreementType.y), AgreementType.y, AgreementType.x),
                DocType = ifelse(!is.na(DocType.y), DocType.y, DocType.x),
                GeogArea = ifelse(!is.na(GeogArea.y), GeogArea.y, GeogArea.x),
                ieaID = ifelse(!is.na(ieaID.y), ieaID.y, ieaID.x),
                gengID = ifelse(!is.na(gengID.y), gengID.y, gengID.x),
                ecolexID = ifelse(!is.na(ecolexID.y), ecolexID.y, ecolexID.x),
                verified = ifelse(!is.na(verified.y), verified.y, verified.x),
                DocValidUntilDate = ifelse(!is.na(DocValidUntilDate.y), DocValidUntilDate.y, DocValidUntilDate.x),
                Notes = ifelse(!is.na(Notes.y), Notes.y, Notes.x),
                Download = ifelse(!is.na(Download.y), Download.y, Download.x),
                MEA_type = ifelse(!is.na(MEA_type.y), MEA_type.y, MEA_type.x),
                Ambit = ifelse(!is.na(Ambit.y), Ambit.y, Ambit.x),
                Region = ifelse(!is.na(Region.y), Region.y, Region.x),
                subject_ecolex = ifelse(!is.na(subject_ecolex.y), subject_ecolex.y, subject_ecolex.x),
                subject_iea = ifelse(!is.na(subject_iea.y), subject_iea.y, subject_iea.x),
                Keywords = ifelse(!is.na(Keywords.y), Keywords.y, Keywords.x),
                Lineage = ifelse(!is.na(Lineage.y), Lineage.y, Lineage.x),
                Sequence = ifelse(!is.na(Sequence.y), Sequence.y, Sequence.x),
                AdoptedIn = ifelse(!is.na(AdoptedIn.y), AdoptedIn.y, AdoptedIn.x),
                Languages = ifelse(!is.na(Languages.y), Languages.y, Languages.x),
                Appendices = ifelse(!is.na(Appendices.y), Appendices.y, Appendices.x),
                Depository = ifelse(!is.na(Depository.y), Depository.y, Depository.x),
                DepositoryURL = ifelse(!is.na(DepositoryURL.y), DepositoryURL.y, DepositoryURL.x),
                Published = ifelse(!is.na(Published.y), Published.y, Published.x),
                Website1 = ifelse(!is.na(Website1.y), Website1.y, Website1.x),
                Website2 = ifelse(!is.na(Website2.y), Website2.y, Website2.y),
                Secretariat = ifelse(!is.na(Secretariat.y), Secretariat.y, Secretariat.x),
                SecretariatURL = ifelse(!is.na(SecretariatURL.y), SecretariatURL.y, SecretariatURL.x),
                UNEP = ifelse(!is.na(UNEP.y), UNEP.y, UNEP.x),
                Supersedes = ifelse(!is.na(Supersedes.y), Supersedes.y, Supersedes.x),
                References = ifelse(!is.na(References.y), References.y, References.x),
                EnabledBy = ifelse(!is.na(EnabledBy.y), EnabledBy.y, EnabledBy.x),
                AmendedBy = ifelse(!is.na(AmendedBy.y), AmendedBy.y, AmendedBy.x),
                Lit = ifelse(!is.na(Lit.y), Lit.y, Lit.x),
                Data = ifelse(!is.na(Data.y), Data.y, Data.x),
                Coded = ifelse(!is.na(Coded.y), Coded.y, Coded.x))%>%
  dplyr::distinct() %>%
  dplyr::select(-c(Title.x, Title.y, Beg.x, Beg.y, End.x, End.y, Signature.x, 
                   Signature.y, Force.x, Force.y, Abstract.x, Abstract.y, 
                   Parties.x, Parties.y, AgreementType.x, AgreementType.y, DocType.y, DocType.x,
                   GeogArea.x, GeogArea.y, gengID.x, gengID.y, ieaID.x, ieaID.y,
                   ecolexID.x, ecolexID.y, verified.x, verified.y,
                   DocValidUntilDate.x, DocValidUntilDate.y, url.x, url.y, 
                   Notes.x, Notes.y, Download.x, Download.y, MEA_type.x, MEA_type.y,
                   Ambit.x, Ambit.y, Region.x, Region.y, subject_ecolex.x, subject_ecolex.y,
                   subject_iea.x, subject_iea.y, Keywords.x, Keywords.y, Lineage.x,
                   Lineage.y, Sequence.x, Sequence.y, AdoptedIn.x, AdoptedIn.y,
                   Languages.x, Languages.y, Appendices.x, Appendices.y, Depository.x,
                   Depository.y, DepositoryURL.x, DepositoryURL.y, Published.x,
                   Published.y, Website1.x, Website1.y,
                   Website2.x, Website2.y, Secretariat.x, Secretariat.y, SecretariatURL.x,
                   SecretariatURL.y, UNEP.x, UNEP.y,Supersedes.x, Supersedes.y,
                   References.x, References.y, EnabledBy.x, EnabledBy.y, AmendedBy.x,
                   AmendedBy.y, Lit.x, Lit.y, Data.x, Data.y, Coded.x, Coded.y,
                   TreatyText.y, TreatyText.x)) %>%   
  dplyr::mutate(Title = manypkgs::standardise_titles(Title)) %>% ## Standardise Title
  dplyr::relocate(manyID, Title, Beg, End, Signature, Force, url, TreatyText,
                  AgreementType, DocType, GeogArea, gengID, ieaID, ecolexID,
                  treatyID, Parties, verified, DocValidUntilDate, Notes, Download,
                  MEA_type, Ambit, Region, subject_ecolex, subject_iea,
                  Keywords, Lineage, Sequence, AdoptedIn, Languages, Appendices,
                  Depository, DepositoryURL, Published, Website1,
                  Website2, Secretariat, SecretariatURL, UNEP, Supersedes,
                  References, EnabledBy, AmendedBy, Lit, Data, Coded) %>%
  arrange(Beg)

## Remove duplicates from merging dataset

## BAD-CHE[LCT]_1884P
which(HUGGO_new$manyID == "BAD-CHE[LCT]_1884P")
## Two different treaties: 1 additional convention and 1 protocol to the additional convention
## Add information of additional convention and change manyID
HUGGO_new[78, 1] <- "BAD-CHE[LCT]_1884A"
HUGGO_new[78, 2] <- "Additional Convention Between Switzerland Baden And Alsace-Lorraine Concerning Fishing In Lake Constance And Its Tributaries"
HUGGO_new[78, 13] <- "IEA-2587"
HUGGO_new[78, 12] <- "GENG-0077"
HUGGO_new[78, 7] <- "https://iea.uoregon.edu/treaty-text/2587"
HUGGO_new[78, 8] <- 0
## Remove duplicates
HUGGO_new <- HUGGO_new[-c(80, 81), ]

## BG07JC_1893A
which(HUGGO_new$manyID == "BG07JC_1893A")
## Duplicate due to different Beg and Signature dates: 1893-08-11 vs. 1893-08-29
## Executed on behalf of Patiala State 25 July 1893; on behalf of the British 
## Government 11 August 1893; approved and confirmed 29 August 1983
## Keep row with 1893-08-29
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "BG07JC_1893A" & HUGGO_new$Beg == "1893-08-11")), ]
## Parties should be GBR-IND, not GBR-PSE
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "BG07JC_1893A" & HUGGO_new$Parties == "GBR-PSE")), ]

## FSHNGB_1894E:FSHNGB_1886A
which(HUGGO_new$manyID == "FSHNGB_1894E:FSHNGB_1886A")
## Same treaty and dates, but duplicate perhaps due to different gengID.
## Keep row with Parties variable. But TreatyID does not correspond with manyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "FSHNGB_1894E:FSHNGB_1886A" & is.na(HUGGO_new$Parties))), ]

## DNK-SWE[FSC]_1902E:DNK-SWE[FSC]_1899A
which(HUGGO_new$manyID == "DNK-SWE[FSC]_1902E:DNK-SWE[FSC]_1899A")
## Same treaty and dates, but duplicate perhaps due different gengID.
## Keep row with Parties variable. But TreatyID does not correspond with manyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "DNK-SWE[FSC]_1902E:DNK-SWE[FSC]_1899A" & is.na(HUGGO_new$Parties))), ]

## HNTLCR_1907E:HNTLCR_1897A
which(HUGGO_new$manyID == "HNTLCR_1907E:HNTLCR_1897A")
## Same treaty and dates, but duplicate perhaps due different gengID.
## Keep row with Parties variable. But TreatyID does not correspond with manyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "HNTLCR_1907E:HNTLCR_1897A" & is.na(HUGGO_new$Parties))), ]

## DNK-SWE[FSC]_1907E:DNK-SWE[FSC]_1899A
which(HUGGO_new$manyID == "DNK-SWE[FSC]_1907E:DNK-SWE[FSC]_1899A")
## Same treaty and dates, but duplicate perhaps due different gengID.
## Keep row with Parties variable. But TreatyID does not correspond with manyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "DNK-SWE[FSC]_1907E:DNK-SWE[FSC]_1899A" & is.na(HUGGO_new$Parties))), ]

## FSHNGB_1908E:FSHNGB_1886A
which(HUGGO_new$manyID == "FSHNGB_1908E:FSHNGB_1886A")
## Same treaty and dates, but duplicate perhaps due different gengID.
## Keep row with Parties variable. But TreatyID does not correspond with manyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "FSHNGB_1908E:FSHNGB_1886A" & is.na(HUGGO_new$Parties))), ]

## HNTLCR_1914E:HNTLCR_1897A
which(HUGGO_new$manyID == "HNTLCR_1914E:HNTLCR_1897A")
## Same treaty and dates, but duplicate perhaps due different gengID.
## Keep row with Parties variable. But TreatyID does not correspond with manyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "HNTLCR_1914E:HNTLCR_1897A" & is.na(HUGGO_new$Parties))), ]

## DEU-DNK[BGF]_1922A
which(HUGGO_new$manyID == "DEU-DNK[BGF]_1922A")
## Three different treaties under the same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "DEU-DNK[BGF]_1922A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## FIN-RUS[FRH]_1922A
which(HUGGO_new$manyID == "FIN-RUS[FRH]_1922A")
## Three different treaties under the same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "FIN-RUS[FRH]_1922A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## FSHNGB_1924E:FSHNGB_1886A
which(HUGGO_new$manyID == "FSHNGB_1924E:FSHNGB_1886A")
## Same treaty and dates, but duplicate perhaps due different gengID.
## Keep row with Parties variable. But TreatyID does not correspond with manyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "FSHNGB_1924E:FSHNGB_1886A" & is.na(HUGGO_new$Parties))), ]

## CAN-USA[LLW]_1925P:CAN-USA[LLW]_1925A
## Repetition of same treaty, but one row has verified information, which is to be kept.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "CAN-USA[LLW]_1925P:CAN-USA[LLW]_1925A" & is.na(HUGGO_new$url))), ]

## HNTLCR_1927E:HNTLCR_1897A
which(HUGGO_new$manyID == "HNTLCR_1927E:HNTLCR_1897A")
## Same treaty and dates, but duplicate perhaps due different gengID.
## Keep row with Parties variable. But TreatyID does not correspond with manyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "HNTLCR_1927E:HNTLCR_1897A" & is.na(HUGGO_new$Parties))), ]

## FIN-NOR[FSP]_1938A
which(HUGGO_new$manyID == "FIN-NOR[FSP]_1938A")
## Two different treaties under the same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "FIN-NOR[FSP]_1938A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## ESAPFC_1948A
which(HUGGO_new$manyID == "ESAPFC_1948A")
## Same treaty under different titles.
## Keep the row with the original title (Council later replaced by Commission)
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ESAPFC_1948A" & !(HUGGO_new$Title == "Agreement For The Establishment Of The Asia Pacific Fisheries Council"))), ]

## AUT-YUG[MBG]_1952P
which(HUGGO_new$manyID == "AUT-YUG[MBG]_1952P")
## Same treaty, duplicate due to different title.
## Keep title with anglicised spelling
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "AUT-YUG[MBG]_1952P" & HUGGO_new$Title == "Summary Protocol Of The Meeting At Velden Between Governments Of Austrian Republic And The Federal Republic Of Yugoslavia")), ]

## OP06SP_1952A
which(HUGGO_new$manyID == "OP06SP_1952A")
## Same treaty. Also repeated in OP07SP_1952A, with different title
which(HUGGO_new$manyID == "OP07SP_1952A")
## Keep row with more information
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "OP06SP_1952A" & is.na(HUGGO_new$Published))), ]
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "OP07SP_1952A")), ]

## FSHNGB_1952E:FSHNGB_1886A
which(HUGGO_new$manyID == "FSHNGB_1952E:FSHNGB_1886A")
## Same treaty and dates, but duplicate perhaps due different gengID.
## Keep row with Parties variable. But TreatyID does not correspond with manyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "FSHNGB_1952E:FSHNGB_1886A" & is.na(HUGGO_new$Parties))), ]

## ESAPFC_1952E:ESAPFC_1948A
which(HUGGO_new$manyID == "ESAPFC_1952E:ESAPFC_1948A")
## Same treaty and dates, but duplicate perhaps due different gengID.
## Keep row with Parties variable. But TreatyID does not correspond with manyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ESAPFC_1952E:ESAPFC_1948A" & is.na(HUGGO_new$Parties))), ]

## IND-NPL[IWS]_1954A
which(HUGGO_new$manyID == "IND-NPL[IWS]_1954A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "IND-NPL[IWS]_1954A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## CDSMMZ_1954P20
which(HUGGO_new$manyID == "CDSMMZ_1954P20")
## Same treaty, fully duplicate row
HUGGO_new <- HUGGO_new[-485, ]

## IND-PAK[TIW]_1955A
which(HUGGO_new$manyID == "IND-PAK[TIW]_1955A")
## Two different treaties under the same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "IND-PAK[TIW]_1955A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## ESAPFC_1955E:ESAPFC_1948A
which(HUGGO_new$manyID == "ESAPFC_1955E:ESAPFC_1948A")
## Same treaty. Keep row with corresponding treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ESAPFC_1955E:ESAPFC_1948A" & !(HUGGO_new$treatyID == "ESAPFC_1955E:ESAPFC_1948A"))), ]

## CAN-IND[CAR]_1956A
which(HUGGO_new$manyID == "CAN-IND[CAR]_1956A")
## Two different treaties under the same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "CAN-IND[CAR]_1956A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## ESAPFC_1958E:ESAPFC_1948A
which(HUGGO_new$manyID == "ESAPFC_1958E:ESAPFC_1948A")
## Same treaty. Keep row with corresponding treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ESAPFC_1958E:ESAPFC_1948A" & !(HUGGO_new$treatyID == "ESAPFC_1958E:ESAPFC_1948A"))), ]

## FSHBBF_1959A
which(HUGGO_new$manyID == "FSHBBF_1959A")
## Two different treaties under the same manyID.
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "FSHBBF_1959A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## ESAPFC_1961E:ESAPFC_1948A
which(HUGGO_new$manyID == "ESAPFC_1961E:ESAPFC_1948A")
## Same treaty. Keep row with corresponding treatyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ESAPFC_1961E:ESAPFC_1948A" & !(HUGGO_new$treatyID == "ESAPFC_1961E:ESAPFC_1948A"))), ]

## INPPSO_1962E:INPPSO_1954A
which(HUGGO_new$manyID == "INPPSO_1962E:INPPSO_1954A")
## Same treaty. Keep row with corresponding treatyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "INPPSO_1962E:INPPSO_1954A" & !(HUGGO_new$treatyID == "INPPSO_1962E:INPPSO_1954A"))), ]

## ESGFCM_1963E:ESGFCM_1949A
which(HUGGO_new$manyID == "ESGFCM_1963E:ESGFCM_1949A")
## Same treaty. Keep row with corresponding treatyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ESGFCM_1963E:ESGFCM_1949A" & !(HUGGO_new$treatyID == "ESGFCM_1963E:ESGFCM_1949A"))), ]

## AR04SN_1963A
which(HUGGO_new$manyID == "AR04SN_1963A")
## Two different treaties.
## Nordic Mutual Emergency already under manyID == NM04RA_1963A
## Keep AR04SN_1963A as Act Regarding Navigation and Economic Cooperation...
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "AR04SN_1963A" & HUGGO_new$treatyID == "NM04RA_1963A")), ]

## ADPLNE_1964P3:PRTLNE_1960A3
which(HUGGO_new$manyID == "ADPLNE_1964P3:PRTLNE_1960A3")
## Two different treaties under the same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "ADPLNE_1964P3:PRTLNE_1960A3"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## TR15IN_1964A1
## Two different treaties under the same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "TR15IN_1964A1"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## RLTDLC_1964A
which(HUGGO_new$manyID == "RLTDLC_1964A")
## Same treaty under different titles: Convention vs. Statutes
## Remove statutes
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "RLTDLC_1964A" & HUGGO_new$Title == "Statutes Relating To The Development Of The Lake Chad Basin")), ]

## EE08AB_1966E:EP07AB_1961A
which(HUGGO_new$manyID == "EE08AB_1966E:EP07AB_1961A")
## Same treaty with different dates.
## Verify dates: https://books.google.fr/books?id=bFYm6Bs7Q6gC&pg=PA714&lpg=PA714&dq=Agreement+Extending+The+Agreement+Effected+By+Exchange+Of+Notes+Concerning+A+Program+To+Study+The+Radioactivity+Of+The+Upper+Atmosphere+By+Means+Of+High-Altitude+Balloons&source=bl&ots=X4o1j7AgJ3&sig=ACfU3U3CXDuf2Ce_kCV0coZWwzV1i4_jhQ&hl=en&sa=X&ved=2ahUKEwjm0puNzb39AhVHxQIHHbMIBxAQ6AF6BAgJEAM#v=onepage&q=Agreement%20Extending%20The%20Agreement%20Effected%20By%20Exchange%20Of%20Notes%20Concerning%20A%20Program%20To%20Study%20The%20Radioactivity%20Of%20The%20Upper%20Atmosphere%20By%20Means%20Of%20High-Altitude%20Balloons&f=false
## Correct Beg = 1966-05-09
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "EE08AB_1966E:EP07AB_1961A" & !(HUGGO_new$Beg== "1966-05-09"))), ]

## JPN-USA[FSF]_1967N
which(HUGGO_new$manyID == "JPN-USA[FSF]_1967N")
## Two different treaties under the same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "JPN-USA[FSF]_1967N"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## DNK-EC[ECH]_1967N
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "DNK-EC[ECH]_1967N"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}


## RUS-USA[FPP]_1967N
which(HUGGO_new$manyID == "RUS-USA[FPP]_1967N")
## Same treaty, duplicate due to different gengID
## Remove one
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "RUS-USA[FPP]_1967N" & HUGGO_new$gengID== "GENG-0814")), ]

## AR06RM_1968P:RU05RM_1968A
which(HUGGO_new$manyID == "	AR06RM_1968P:RU05RM_1968A")
## Same treaty. Keep row with corresponding treatyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "AR06RM_1968P:RU05RM_1968A" & !(HUGGO_new$treatyID == "AR06RM_1968P:RU05RM_1968A"))), ]

## ESP-PRT[FSF]_1969A
which(HUGGO_new$manyID == "ESP-PRT[FSF]_1969A")
## Same treaty repeated
## Remove two duplicates
HUGGO_new <- HUGGO_new[-c(939, 940), ]

## PRTSBS_1972P:PRTSBS_1962A
which(HUGGO_new$manyID == "PRTSBS_1972P:PRTSBS_1962A")
## Same treaty but different dates
## Keep row with End
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "PRTSBS_1972P:PRTSBS_1962A" & !is.na(HUGGO_new$End))), ]

## ELCCDF_1972A
which(HUGGO_new$manyID == "ELCCDF_1972A")
## Same treaty. Keep row with url
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ELCCDF_1972A" & !is.na(HUGGO_new$url))), ]

## RUS-USA[CSC]_1973A
which(HUGGO_new$manyID == "RUS-USA[CSC]_1973A")
## Three different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "RUS-USA[CSC]_1973A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## RUS-USA[PFC]_1973A
which(HUGGO_new$manyID == "RUS-USA[PFC]_1973A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "RUS-USA[PFC]_1973A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## CAN-NOR[CSS]_1975N
which(HUGGO_new$manyID == "CAN-NOR[CSS]_1975N")
## Two different treaties under same manyID
## Remove first duplicates
HUGGO_new <- HUGGO_new[-c(1256, 1300), ]
## Change manyID of later treaty to differentiate them
which(HUGGO_new$manyID == "CAN-NOR[CSS]_1975N")
HUGGO_new[1298, 1] <- "CAN-NOR[CSS]_1975N2"

## GU14US_1975A
which(HUGGO_new$manyID == "GU14US_1975A")
## Two different treaties under the same manyID
## Change manyID of later treaty to differentiate them
HUGGO_new[1299, 1] <- "GU14US_1975A2"
## Remove duplicates
HUGGO_new <- HUGGO_new[-c(1265, 1300), ]

## RU05RM_1976P2:RU05RM_1968A
which(HUGGO_new$manyID == "RU05RM_1976P2:RU05RM_1968A")
## Same treaty. Keep row with corresponding treatyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "RU05RM_1976P2:RU05RM_1968A" & !(HUGGO_new$treatyID == "RU05RM_1976P2:RU05RM_1968A"))), ]

## PRTMSP_1976A
which(HUGGO_new$manyID == "PRTMSP_1976A")
## Two different treaties under the same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "PRTMSP_1976A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## ESGFCM_1976E:ESGFCM_1949A
which(HUGGO_new$manyID == "ESGFCM_1976E:ESGFCM_1949A")
## Same treaty. Keep row with corresponding treatyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ESGFCM_1976E:ESGFCM_1949A" & !(HUGGO_new$treatyID == "ESGFCM_1976E:ESGFCM_1949A"))), ]

## CL07SM_1977A
which(HUGGO_new$manyID == "CL07SM_1977A")
## Two different treaties under the same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "CL07SM_1977A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## GUY-RUS[UNF]_1977A
which(HUGGO$manyID == "GUY-RUS[UNF]_1977A")
## Same treaty. Remove one row.
HUGGO_new <- HUGGO_new[-1426, ]

## ESAPFC_1977E:ESAPFC_1948A
which(HUGGO_new$manyID == "ESAPFC_1977E:ESAPFC_1948A")
## Same treaty. Keep row with corresponding treatyID.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ESAPFC_1977E:ESAPFC_1948A" & !(HUGGO_new$treatyID == "ESAPFC_1977E:ESAPFC_1948A"))), ]

## BGR-RUS[BSA]_1978A
which(HUGGO_new$manyID == "BGR-RUS[BSA]_1978A")
## Two different treaties under the same manyID.
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "BGR-RUS[BSA]_1978A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## DEU-ESP[GSE]_1978A
which(HUGGO_new$manyID == "DEU-ESP[GSE]_1978A")
## Two different treaties under the same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "DEU-ESP[GSE]_1978A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## IR05GS_1979P:IR05GS_1957A
which(HUGGO$manyID == "IR05GS_1979P:IR05GS_1957A")
## Same treaty. Keep row with corresponding url.
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "IR05GS_1979P:IR05GS_1957A" & !is.na(HUGGO_new$url))), ]

## ISL-NOR[CSQ]_1980A
which(HUGGO_new$manyID == "ISL-NOR[CSQ]_1980A")
## Two different treaties, and repetition of one.
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "ISL-NOR[CSQ]_1980A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}
## Remove duplicates left
which(HUGGO_new$manyID == "ISL-NOR[CSQ]_1980A")
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ISL-NOR[CSQ]_1980A" & HUGGO_new$Title == "Bilateral Agreement Between Norway And Iceland Concerning Fisheries And Continental Shelf Questions")), ]

## NOR-EC[ECF]_1980A
which(HUGGO_new$manyID == "NOR-EC[ECF]_1980A")
## Same treaty with different titles
## Keep row with url and verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "NOR-EC[ECF]_1980A" & !is.na(HUGGO_new$url))), ]

## DEU-LUX[FFA]_1980N
which(HUGGO_new$manyID == "DEU-LUX[FFA]_1980N")
## Duplicates of same treaty
## Remove one row
HUGGO_new <- HUGGO_new[-1638, ]

## ESPOTF_1983A 
which(HUGGO_new$manyID == "ESPOTF_1983A")
## Duplicates of the same treaty
## Remove one row
HUGGO_new <- HUGGO_new[-1738, ]

## HUN-ROU[FCB]_1986A
which(HUGGO_new$manyID == "HUN-ROU[FCB]_1986A")
## Duplicates of the same treaty
## Remove one row
HUGGO_new <- HUGGO_new[-1878, ]

## GTM-MEX[PEA]_1987A
which(HUGGO_new$manyID == "GTM-MEX[PEA]_1987A")
## Duplicate of same treaty, but one row with incorrect parties: GTM-USA
## Keep row with correct parties: GTM-MEX
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "GTM-MEX[PEA]_1987A" & HUGGO_new$Parties == "GTM-USA")), ]

## ZMB-ZWE[UTZ]_1987A
which(HUGGO_new$manyID == "ZMB-ZWE[UTZ]_1987A")
## Duplicate of same treaty, but one row with incorrect parties: ZAF-ZMB
## Keep row with correct parties: ZMB-ZWE
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ZMB-ZWE[UTZ]_1987A" & HUGGO_new$Parties == "ZAF-ZMB")), ]

## ARG-URY[HHS]_1987A
which(HUGGO_new$manyID == "ARG-URY[HHS]_1987A")
## Duplicate of same treaty, but different titles, and different treatyID
## Keep row with verified information
HUGGO_new <- HUGGO_new[-1939, ]


## BCIPPS_1989E:MARPOL_1973A
which(HUGGO_new$manyID == "BCIPPS_1989E:MARPOL_1973A")
## Same treaty under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "BCIPPS_1989E:MARPOL_1973A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## CAN-USA[FCS]_1989A
which(HUGGO_new$manyID == "CAN-USA[FCS]_1989A")
## Duplicate of same treaty
## Remove one row
HUGGO_new <- HUGGO_new[-2039, ]

## AR07PS_1990E1:MARPOL_1973A
which(HUGGO_new$manyID == "AR07PS_1990E1:MARPOL_1973A")
## Two different treaties under same manyID
## Change manyID of later treaty to differentiate them
HUGGO_new[2115, 1] <- "AR07PS_1990E2:MARPOL_1973A"

## RB04PS_1990E:MARPOL_1973A
which(HUGGO_new$manyID == "RB04PS_1990E:MARPOL_1973A")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[2062, 1] <- "RB04PS_1990E1:MARPOL_1973A"

## AUS-JPN[LLF]_1990P
which(HUGGO_new$manyID == "AUS-JPN[LLF]_1990P")
## Duplicate of same treaty
## Remove one row
HUGGO_new <- HUGGO_new[-2121, ]

## ESONPR_1990A
which(HUGGO_new$manyID == "ESONPR_1990A")
## Same treaty with different titles
## Remove one row
HUGGO_new <- HUGGO_new[-2122, ]

## TPNWLA_1991E:TPNWLA_1967A
which(HUGGO_new$manyID == "TPNWLA_1991E:TPNWLA_1967A")
## Same treaty, different treatyID
## Keep row with verified metadata, but differing treatyID
HUGGO_new <- HUGGO_new[-2146, ]

## KOR-USA[CNF]_1991A
which(HUGGO_new$manyID == "KOR-USA[CNF]_1991A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "KOR-USA[CNF]_1991A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## INTCPE_1991P:INTCPE_1990A
which(HUGGO_new$manyID == "INTCPE_1991P:INTCPE_1990A")
## Same treaty. Keep row with corresponding treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "INTCPE_1991P:INTCPE_1990A" & HUGGO_new$treatyID == "INTCPE_1991P:INTCPE_1990A")), ]

## ANIPPS_1992E1:MARPOL_1973A
which(HUGGO_new$manyID == "ANIPPS_1992E1:MARPOL_1973A")
## Same treaty, but different gengID and url
## Keep one row
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ANIPPS_1992E1:MARPOL_1973A" & HUGGO_new$gengID == "MGENG-0489")), ]

## SWZ-ZAF[DUK]_1992A
which(HUGGO_new$manyID == "SWZ-ZAF[DUK]_1992A")
## Duplicate of same treaty
## Keep one row
HUGGO_new <- HUGGO_new[-2213, ]

## AGO-EC[CPF]_1992P
which(HUGGO_new$manyID == "AGO-EC[CPF]_1992P")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[2221, 1] <- "AGO-EC[CPF]_1992P2"

## DEU-RUS[GEP]_1992A
which(HUGGO_new$manyID == "DEU-RUS[GEP]_1992A")
## Same treaty, different titles
## Keep row with shorter title
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "DEU-RUS[GEP]_1992A" & HUGGO_new$Title == "Agreement Between The Ministry Of Ecology And Natural Resources Of The Russian Federation And The Federal Ministry Of Environment Conservation And Safety Of Nuclear Reactors Of The Republic Of Germany About Assistance To Development Of The Economic Cooperation Directed On The Solution Of Problems In The Field Of Environmental Protection (about Creation Of Ecological Bureau)")), ]

## RUS-TJK[GIR]_1993A
which(HUGGO_new$manyID == "RUS-TJK[GIR]_1993A")
## Same treaty, different titles
## Keep row with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "RUS-TJK[GIR]_1993A" & is.na(HUGGO_new$url))), ]

## RUS-UKR[FSA]_1993A
which(HUGGO_new$manyID == "RUS-UKR[FSA]_1993A")
## Two different treaty under same manyID, and one duplicate
## Remove duplicate
## Keep row with verified metadata
HUGGO_new <- HUGGO_new[-2336, ]
## Change manyID of one treaty to differentiate them
which(HUGGO_new$manyID == "RUS-UKR[FSA]_1993A")
HUGGO_new[2336, 1] <- "RUS-UKR[OGS]_1993A"

## EST-FIN[ARC]_1993A
which(HUGGO_new$manyID == "EST-FIN[ARC]_1993A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "EST-FIN[ARC]_1993A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## ESSRCF_1993E:ESSRCF_1985A
which(HUGGO_new$manyID == "ESSRCF_1993E:ESSRCF_1985A")
## Same treaty
## Keep row with corresponding treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ESSRCF_1993E:ESSRCF_1985A" & HUGGO_new$treatyID == "ESSRCF_1993E:ESSRCF_1985A")), ]

## GBR-USA[BVI]_1993A
which(HUGGO_new$manyID == "GBR-USA[BVI]_1993A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "GBR-USA[BVI]_1993A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## RA04WM_1993E1
which(HUGGO_new$manyID == "RA04WM_1993E1")
## Same treaty
## Remove one row
HUGGO_new <- HUGGO_new[-2382, ]

## EB06DB_1993A
which(HUGGO_new$manyID == "EB06DB_1993A")
## Duplicate of same treaty
## Remove one row
HUGGO_new <- HUGGO_new[-2384, ]

## ESAPFC_1993E:ESAPFC_1948A
which(HUGGO_new$manyID == "ESAPFC_1993E:ESAPFC_1948A")
## Same treaty, keep row with corresponding treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ESAPFC_1993E:ESAPFC_1948A" & HUGGO_new$treatyID == "ESAPFC_1993E:ESAPFC_1948A")), ]

## CAN-EC[FFR]_1993A
which(HUGGO_new$manyID == "CAN-EC[FFR]_1993A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "CAN-EC[FFR]_1993A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## EST-RUS[FRF]_1994A
which(HUGGO_new$manyID == "EST-RUS[FRF]_1994A")
## Same treaty, duplicate due to different titles and gengID
## Keep row with shorter title
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "EST-RUS[FRF]_1994A" & HUGGO_new$Title == "International Agreement Between The Government Of The Russian Federation And The Government Of The Republic Of Estonia Regarding Cooperation In The Sphere Of Fisheries (1994)")), ]

## BLR-RUS[FEI]_1994A
which(HUGGO_new$manyID == "BLR-RUS[FEI]_1994A")
## Two different treaties under same manyID
## Change manyID with the available treatyID of one treaty to differentiate change
HUGGO_new[2491, 1] <- "BLR-RUS[FES]_1994A"

## BJ06TP_1995A
which(HUGGO_new$manyID == "BJ06TP_1995A")
## Two different treaties under same manyID
## Remove duplicates
HUGGO_new <- HUGGO_new[-c(2540, 2541, 2542, 2556, 2557, 2558), ]
## Change manyID of later treaty to differentiate them
which(HUGGO_new$manyID == "BJ06TP_1995A")
HUGGO_new[2552, 1] <- "BJ06TP_1995A2"

## EXEMLR_1995E:ENVMLR_1993A
which(HUGGO_new$manyID == "EXEMLR_1995E:ENVMLR_1993A")
## Same treaty under different titles
## Keep row with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "EXEMLR_1995E:ENVMLR_1993A" & is.na(HUGGO_new$url))), ]

## RUS-UKR[OPP]_1995A
which(HUGGO_new$manyID == "RUS-UKR[OPP]_1995A")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[2588, 1] <- "RUS-UKR[ENV]_1995A"

## CTMHWD_1995E:CTMHWD_1989A
which(HUGGO_new$manyID == "CTMHWD_1995E:CTMHWD_1989A")
## Same treaty
## Keep row with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "CTMHWD_1995E:CTMHWD_1989A" & is.na(HUGGO_new$url))), ]

## GIN-EC[ECF]_1995P
which(HUGGO_new$manyID == "GIN-EC[ECF]_1995P")
## Same treaty
## Keep one row
HUGGO_new <- HUGGO_new[-2612, ]

## CS07IW_1996A
which(HUGGO_new$manyID == "CS07IW_1996A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "CS07IW_1996A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## SVK-YUG[SQP]_1996A
which(HUGGO_new$manyID == "SVK-YUG[SQP]_1996A")
## Two different treaties under same manyID
## Change manyID one treaty to differentiate them
HUGGO_new[2696, 1] <- "SVK-YUG[VTM]_1996A"

## ARG-PRY[LPN]_1996A
which(HUGGO_new$manyID == "ARG-PRY[LPN]_1996A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "ARG-PRY[LPN]_1996A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## ESADED_1997A
which(HUGGO_new$manyID == "ESADED_1997A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "ESADED_1997A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## MKD-YUG[MPQ]_1997A
which(HUGGO_new$manyID == "MKD-YUG[MPQ]_1997A")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[2749, 1] <- "MKD-YUG[VTM]_1997A"

## BRA-URY[NDC]_1997P:BRA-URY[NDC]_1991A
which(HUGGO_new$manyID == "BRA-URY[NDC]_1997P:BRA-URY[NDC]_1991A")
## Same treaty, duplicate because of different titles
## Keep row with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "BRA-URY[NDC]_1997P:BRA-URY[NDC]_1991A" & HUGGO_new$treatyID == "BRA-URY[NDC]_1997P:BRA-URY[NDC]_1991A")), ]

## HUN-ROU[ENP]_1997A
which(HUGGO_new$manyID == "HUN-ROU[ENP]_1997A")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[2762, 1] <- "HUN-ROU[NNA]_1997A"

## CYP-CZE[AGR]_1997A
which(HUGGO_new$manyID == "CYP-CZE[AGR]_1997A")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[2771, 1] <- "CYP-CZE[VTM]_1997A"

## AA08MP_1997N9
which(HUGGO_new$manyID == "AA08MP_1997N9")
## Two different treaties under same manyID
## Change manyID of later treaty to differentiate them
HUGGO_new[2801, 1] <- "AA08MP_1997N1"

## LTU-RUS[FRS]_1997A
which(HUGGO_new$manyID == "LTU-RUS[FRS]_1997A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "LTU-RUS[FRS]_1997A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## CHN-RUS[ABR]_1997A
which(HUGGO_new$manyID == "CHN-RUS[ABR]_1997A")
## Repetition of same treaty
## Remove one row
HUGGO_new <- HUGGO_new[-2814, ]

## BLR-IRN[IQP]_1998A
which(HUGGO_new$manyID == "BLR-IRN[IQP]_1998A")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[2854, 1] <- "BLR-IRN[VTM]_1998A" 

## IMPLAE_1998P
which(HUGGO_new$manyID == "IMPLAE_1998P")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[2904, 1] <- "IMPLAE_1998P1"

## CZE-HRV[PRT]_1998A
which(HUGGO_new$manyID == "CZE-HRV[PRT]_1998A")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[2923, 1] <- "CZE-HRV[VTM]_1998A"

## LTU-RUS[RFE]_1999A
which(HUGGO_new$manyID == "LTU-RUS[RFE]_1999A")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[2972, 1] <- "LTU-RUS[RFF]_1999A"

## CE06NS_1999A
which(HUGGO_new$manyID == "CE06NS_1999A")
## Two different treaties under same manyID
## Remove duplicates
HUGGO_new <- HUGGO_new[-c(2980, 2981), ]
## Change manyID of one treaty to differentiate them
which(HUGGO_new$manyID == "CE06NS_1999A")
HUGGO_new[2981, 1] <- "CE06NS_1999A2"

## LC06WD_1999P
which(HUGGO_new$manyID == "LC06WD_1999P")
## Two different treaties under same manyID
## Protocol on Wildlife... already present under WLDCLE_1999P
## Change manyID to WLDCLE_1999P but remove previous containing unverified info
which(HUGGO_new$manyID == "WLDCLE_1999P")
HUGGO_new <- HUGGO_new[-2985, ]
which(HUGGO_new$manyID == "LC06WD_1999P")
HUGGO_new[2984, 1] <- "WLDCLE_1999P"

## AA08MP_1999N11
which(HUGGO_new$manyID == "AA08MP_1999N11")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[3005, 1] <- "AA08MP_1999N12"

## KAZ-KGZ[CTR]_2000A
which(HUGGO_new$manyID == "KAZ-KGZ[CTR]_2000A")
## Same treaty under different titles and different treatyID, and one different treaty
## Remove one duplicate
HUGGO_new <- HUGGO_new[-3011, ]
## Change manyID to differentiate the treaty left
which(HUGGO_new$manyID == "KAZ-KGZ[CTR]_2000A")
HUGGO_new[3032, 1] <- HUGGO_new[3032, 15]

## AGO-EC[CPF]_2000P
which(HUGGO_new$manyID == "AGO-EC[CPF]_2000P")
## Two different treaties under the same manyID
## Remove duplicates
HUGGO_new <- HUGGO_new[-c(3027, 3045), ]
## Change manyID of later treaty to distinguish it
which(HUGGO_new$manyID == "AGO-EC[CPF]_2000P")
HUGGO_new[3044, 1] <- "AGO-EC[CPF]_2000P1"

## ARG-VEN[ENA]_2000A
which(HUGGO_new$manyID == "ARG-VEN[ENA]_2000A")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[3047, 1] <- "ARG-VEN[MNA]_2000A"

## ITA-SMR[SMT]_2000A
which(HUGGO_new$manyID == "ITA-SMR[SMT]_2000A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "ITA-SMR[SMT]_2000A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## BLR-UKR[DRS]_2001A
which(HUGGO_new$manyID == "BLR-UKR[DRS]_2001A")
## 5 different treaties under the same manyID
## Change manyID to differentiate them
HUGGO_new[3148, 1] <- "BLR-UKR[ERE]_2001A"
HUGGO_new[3149, 1] <- "BLR-UKR[SRS]_2001A"
HUGGO_new[3150, 1] <- "BLR-UKR[PTW]_2001A"
HUGGO_new[3164, 1] <- "BLR-UKR[PTA]_2001A"

## CD08AI_2001E:CD05HS_1983A
which(HUGGO_new$manyID == "CD08AI_2001E:CD05HS_1983A")
## Same treaty, duplicate due to different gengID and ieaID
## Remove one row, keep the one with verified info
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "CD08AI_2001E:CD05HS_1983A" & is.na(HUGGO_new$url))), ]

## CHN-TUN[DTM]_2002A
which(HUGGO_new$manyID == "CHN-TUN[DTM]_2002A")
## Same treaty, duplicate due to different title
## Remove row with title in French
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "CHN-TUN[DTM]_2002A" & HUGGO_new$Title == "Convention Entre Le Gouvernement De La RA(100)publique Tunisienne Et Le Gouvernement De La RA(100)publique Populaire De Chine Dans Le Domaine Du Transport Maritime")), ]

## SEN-EC[CFP]_2002A
which(HUGGO_new$manyID == "SEN-EC[CFP]_2002A")
## Two different treaties under same manyID
## Remove duplicates
HUGGO_new <- HUGGO_new[-c(3213, 3214), ]
## Change manyID to differentiate between treaties
which(HUGGO_new$manyID == "SEN-EC[CFP]_2002A")
HUGGO_new[3212, 1] <- "SEN-EC[CFP]_2002A2"

## INTRRM_2002A
which(HUGGO_new$manyID == "INTRRM_2002A")
## Same treaty, duplicate due to different gengID
## Remove one row
HUGGO_new <- HUGGO_new[-3247, ]

## CRI-USA[TTE]_2003A
which(HUGGO_new$manyID == "CRI-USA[TTE]_2003A")
## Same treaty
## Remove one row
HUGGO_new <- HUGGO_new[-3309, ]

## FC04SP_2003P:FC04SP_2000A
which(HUGGO_new$manyID == "FC04SP_2003P:FC04SP_2000A")
## Same treaty
## Keep row with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "FC04SP_2003P:FC04SP_2000A" & is.na(HUGGO_new$url))), ]

## BLR-SYR[SAI]_2003A
which(HUGGO_new$manyID == "BLR-SYR[SAI]_2003A")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[3324, 1] <- "BLR-SYR[VTQ]_2003A"

## CHN-KAZ[PCE]_2004A
which(HUGGO_new$manyID == "CHN-KAZ[PCE]_2004A")
## Two different treaties under same manyID
## Change manyID of one treaty to differentiate them
HUGGO_new[3352, 1] <- "CHN-KAZ[CEC]_2004A"

## ID04MN_2004E8
which(HUGGO_new$manyID == "ID04MN_2004E8")
## These are different treaties, although they show up as the same
## Add missing treaty
HUGGO_new[3386, 2] <- "Amendments To The Agreement On The International Dolphin Conservation Program (amending Annex 8 (1) Adopted At Meeting Number 12)"
HUGGO_new[3386, 17] <- NA
## Change manyID to differentiate them
HUGGO_new[3386, 1] <- "ID04MN_2004E9"

## FCNEAF_2004E:FCNEAF_1980A
which(HUGGO_new$manyID == "FCNEAF_2004E:FCNEAF_1980A")
## Same treaty, keep row with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "FCNEAF_2004E:FCNEAF_1980A" & is.na(HUGGO_new$url))), ]

## HND-MEX[EUM]_2005A
which(HUGGO_new$manyID == "HND-MEX[EUM]_2005A")
## Same treaty, duplicate due to different titles and gengID
## Keep row with title in English
HUGGO_new <- HUGGO_new[-c(which(HUGGO_new$manyID == "HND-MEX[EUM]_2005A" & is.na(HUGGO_new$AgreementType))), ]

## FC05PA_2006E1:FCNEAF_1980A
which(HUGGO_new$manyID == "FC05PA_2006E1:FCNEAF_1980A")
## Same treaty, duplicate due to different gengID and treatyID
## Keep row with corresponding treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "FC05PA_2006E1:FCNEAF_1980A" & !(HUGGO_new$treatyID == "FC05PA_2006E1:FCNEAF_1980A"))), ]

## BRA-PRY[DRA]_2006A
which(HUGGO_new$manyID == "BRA-PRY[DRA]_2006A")
## Same treaty, duplicate due to different title
## Keep row with title in English
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "BRA-PRY[DRA]_2006A" & !(HUGGO_new$Title == "Cooperation Agreement Between The Government Of The Republic Of Paraguay And The Government Of The Federative Republic Of Brazil For Sustainable Development And Integrated Management Of The Apa River Basin"))), ]

## AE04CC_2007P:AE04CC_1999A
which(HUGGO_new$manyID == "AE04CC_2007P:AE04CC_1999A")
## Same treaty, duplicate due to different gengID
## Keep row with corresponding treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "AE04CC_2007P:AE04CC_1999A" & HUGGO_new$treatyID == "AE04CC_2007P:AE04CC_1999A")), ]

## ARG-ECU[AHE]_2007A
which(HUGGO_new$manyID == "ARG-ECU[AHE]_2007A")
## Same treaty under different title
## Keep row with Eglish title
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ARG-ECU[AHE]_2007A" & !is.na(HUGGO_new$Parties))), ]

## DNK-EC[RGH]_2007A1:DNK-EC[RGH]_2007A1
which(HUGGO_new$manyID == "DNK-EC[RGH]_2007A1:DNK-EC[RGH]_2007A1")
## Same treaty.
## Remove two rows
HUGGO_new <- HUGGO_new[-c(3572, 3573), ]

## ESP-PRT[PHB]_2008P:ESP-PRT[PHB]_2000A
which(HUGGO_new$manyID == "ESP-PRT[PHB]_2008P:ESP-PRT[PHB]_2000A")
## Same treaty. 
## Keep row with corresponding treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ESP-PRT[PHB]_2008P:ESP-PRT[PHB]_2000A" & !(HUGGO_new$treatyID == "ESP-PRT[PHB]_2008P:ESP-PRT[PHB]_2000A"))), ]

## AC06AP_2008P:CP05AP_1990A
which(HUGGO_new$manyID == "AC06AP_2008P:CP05AP_1990A")
## Same treaty.
## Keep row with corresponding treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "AC06AP_2008P:CP05AP_1990A" & !(HUGGO_new$treatyID == "AC06AP_2008P:CP05AP_1990A"))), ]

## BRA-CRI[ABP]_2008P
which(HUGGO_new$manyID == "BRA-CRI[ABP]_2008P")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "BRA-CRI[ABP]_2008P"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## BRA-CRI[DCE]_2008P
which(HUGGO_new$manyID == "BRA-CRI[DCE]_2008P")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "BRA-CRI[DCE]_2008P"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## ET04AB_2008A
which(HUGGO_new$manyID == "ET04AB_2008A")
## Same treaty, duplication due to different gengID
## Keep one row
HUGGO_new <- HUGGO_new[-3654, ]

## ENCNZC_2010A
which(HUGGO_new$manyID == "ENCNZC_2010A")
## Same treaty, duplication due to different gengID
## Keep row with better structured title
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ENCNZC_2010A" & HUGGO_new$Title == "New Zealand Hong Kong China Environment Cooperation Agreement")), ]

## AC08IW_2010E2:CS07IW_1996A
which(HUGGO_new$manyID == "AC08IW_2010E2:CS07IW_1996A")
## Same treaty, keep row with corresponding treatyID
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "AC08IW_2010E2:CS07IW_1996A" & !(HUGGO_new$treatyID == "AC08IW_2010E2:CS07IW_1996A"))), ]

## ARG-URY[AEA]_2010A
which(HUGGO_new$manyID == "ARG-URY[AEA]_2010A")
## Two different treaties under same manyID
## Change manyID of one to differentiate them
HUGGO_new[3740, 1] <- "ARG-URY[ABI]_2010A" 

## AC08IW_2012E1:CS07IW_1996A
which(HUGGO_new$manyID == "AC08IW_2012E1:CS07IW_1996A")
## Same treaty under different titles
## Keep one row with correct title
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "AC08IW_2012E1:CS07IW_1996A" & HUGGO_new$Title == "Amendments To Annexes 1 And 2 To The Convention Concerning The Collection Storage And Discharge Of Waste From Ships Navigating Along The Rhine And Other Inland Waters")), ]

## GUY-RUS[UNF]_1977A
which(HUGGO_new$manyID == "GUY-RUS[UNF]_1977A")
## Same treaty
## Keep one row
HUGGO_new <- HUGGO_new[-1415, ]

## Check whether cleaning created more duplicates

testdup <- HUGGO_new %>% 
  dplyr::group_by(manyID) %>%
  dplyr::filter(n()>1) %>%
  arrange(Beg)

## Clean remaining duplicates

## DEU-DNK[RFF]_1922A
which(HUGGO_new$manyID == "DEU-DNK[GDF]_1922A")
## Two different treaties under same manyID
## Replace manyID of each one with their treatyID, which are different
i <- 0
for (i in 1:nrow(HUGGO_new)){
  if (HUGGO_new[i, 1] == "DEU-DNK[GDF]_1922A"){
    HUGGO_new[i, 1] <- HUGGO_new[i, 15]
  }
}

## FIN-RUS[LLH]_1922A
which(HUGGO_new$manyID == "FIN-RUS[LLH]_1922A")
## Same treaty
## Keep row with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "FIN-RUS[LLH]_1922A" & is.na(HUGGO_new$Force))), ]

## FIN-RUS[PFR]_1922A
which(HUGGO_new$manyID == "FIN-RUS[PFR]_1922A")
## Same treaty
## Keep row with verified metadata 
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "FIN-RUS[PFR]_1922A" & is.na(HUGGO_new$url))), ]

## GBR-USA[VIA]_1993A
which(HUGGO_new$manyID == "GBR-USA[VIA]_1993A")
## Same treaty
## Keep row with updated url
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "GBR-USA[VIA]_1993A" & HUGGO_new$url == "http://www.ecolex.org/server2.php/libcat/docs/TRE/Full/Other/TRE-153638.pdf")), ]

## ARG-PRY[SPR]_1996A
which(HUGGO_new$manyID == "ARG-PRY[SPR]_1996A")
## Same treaty
## Keep row with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ARG-PRY[SPR]_1996A" & is.na(HUGGO_new$url))), ]

## HUN-ROU[NNA]_1997A
which(HUGGO_new$manyID == "HUN-ROU[NNA]_1997A")
## Same treaty 
## Keep row with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "HUN-ROU[NNA]_1997A" & is.na(HUGGO_new$url))), ]

## LTU-RUS[RFF]_1999A
which(HUGGO_new$manyID == "LTU-RUS[RFF]_1999A")
## Same treaty
## Keep row with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "LTU-RUS[RFF]_1999A" & is.na(HUGGO_new$url))), ]

# Stage four: Connecting data
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
