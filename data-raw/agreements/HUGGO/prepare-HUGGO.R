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

# Stage three: Merging verified data and additional treaties into HUGGO dataset

# Step one: Merge data frames with verified metadata of treaties within and
# without HUGGO

# Load original HUGGO
HUGGO_or <- manyenviron::agreements$HUGGO

# Load data frame with verified metadata (of treaties present in HUGGO)
HUGGO7 <- read.csv("data-raw/agreements/HUGGO/HUGGO_verified.csv", sep = ";", 
                   na.strings = "#N/D")

# Load data frame with verified metadata (of treaties not present in HUGGO)
HUGGO8 <- read.csv("data-raw/agreements/HUGGO/HUGGO_additional.csv", sep = ";", 
                   na.strings = "#N/D")

# Create one data frame
HUGGO9 <- rbind(HUGGO7, HUGGO8)

# Drop text columns and the ones included for verification purposes
HUGGO9 <- HUGGO9[, -c(47:51)]

# Add column to indicate if the treaty text has been collected
HUGGO9 <- HUGGO9 %>%
  dplyr::mutate(TreatyText = 1)
HUGGO_or <- HUGGO_or %>%
  dplyr::mutate(TreatyText = NA)

# Indicate which treaty texts were not/could not be collected
misstxt <- c("ADDTNR_1979P2", "AGO-EC[PFW]_1999P", "ARG-URY[HHS]_1987A", 
             "ARG-VEN[ENA]_2000A", "BAD-CHE[LCT]_1884P", "BJ06TP_1995A",
             "BLR-EGY[QPT]_1998A", "BLR-SYR[SAI]_2003A", "BRA-CRI[CET]_2008P",
             "CC07AR_1993A", "CE04FU_1987A", "CL07SM_1977A", 
             "CNSRBE_1995E:CNSRBE_1991A", "CSCBNS_1992A", "CYP-EUE[ION]_2000P5",
             "DEU-GTM[AML]_2012A", "EGY-ALG[ANH]_1998A", "EGY-JOR[FSH]_1998A",
             "EGY-MAR[VAH]_1999A", "ELCCDF_1972A", "EP05SP_1998P:EP05SP_1986A",
             "EUE-SLB[FSH]_2010A", "GNB-EC[FSP]_2008A", "HUN-RUS[RAF]_1950A",
             "ICAAAN_1944A16", "ID04MN_2004E8", "MNG-RUS[AIF]_2002A",
             "MU09EM_1998S", "PU05MD_1997A", "RF04RR_1933A", "RUS-USA[UNF]_1975A",
             "RUS-USA[HSA]_1968E:RUS-USA[HSA]_1967A", "SEN-EC[CFP]_2002A",
             "SI05SC_2007P", "SYC-EC[CPF]_2011P", "TC10FC_2005A", "TUR-ALG[MAF]_1999A")
i <- 0
for(i in 1:length(misstxt)){
  HUGGO9[which(HUGGO9$manyID == misstxt[i]), 47] <- 0
}

# Merge data frames
HUGGO_new <- dplyr::full_join(HUGGO_or, HUGGO9, by = c("manyID", "treatyID")) %>%
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
  dplyr::arrange(Beg)

# Step two: Clean duplicate rows

# First row NA remove
HUGGO_new <- HUGGO_new[-(which(is.na(HUGGO_new$manyID))), ]

# BAD-CHE[LCT]_1884P
# Additional Convention Between Switzerland Baden And Alsace-Lorraine Concerning Fishing In Lake Constance And Its Tributaries
which(HUGGO_new$manyID == "BAD-CHE[LCT]_1884P")
remove <- which(HUGGO_new$manyID == "BAD-CHE[LCT]_1884P" & HUGGO_new$Title == "Additional Convention Between Switzerland Baden And Alsace-Lorraine Concerning Fishing In Lake Constance And Its Tributaries")
# Keep one row
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
# Protocol To The Additional Convention Between Switzerland Baden And Alsace-Lorraine Concerning Fishing In Lake Constance And Its Tributaries
which(HUGGO_new$manyID == "BAD-CHE[LCT]_1884P")
remove <- which(HUGGO_new$manyID == "BAD-CHE[LCT]_1884P" & HUGGO_new$Title == "Protocol To The Additional Convention Between Switzerland Baden And Alsace-Lorraine Concerning Fishing In Lake Constance And Its Tributaries")
# Keep one row
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# CAN-USA[LLW]_1925P:CAN-USA[LLW]_1925A
which(HUGGO_new$manyID == "CAN-USA[LLW]_1925P:CAN-USA[LLW]_1925A")
# Keep row with verified information (Force)
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "CAN-USA[LLW]_1925P:CAN-USA[LLW]_1925A" & HUGGO_new$Force == "1925-07-17")), ]

# OP06SP_1952A
which(HUGGO_new$manyID == "OP06SP_1952A")
# Keep row with more information
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "OP06SP_1952A" & !is.na(HUGGO_new$Region))), ]

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
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "ELCCDF_1972A" & is.na(HUGGO_new$TreatyText))), ]

# RUS-USA[HSA]_1975A
which(HUGGO_new$manyID == "RUS-USA[HSA]_1975A")
# Two fully duplicated treaties, remove duplicated rows
remove <- which(HUGGO_new$manyID == "RUS-USA[HSA]_1975A" & HUGGO_new$gengID == "GENG-1181")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "RUS-USA[HSA]_1975A" & HUGGO_new$gengID == "GENG-1248")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# CAN-NOR[CSS]_1975N
which(HUGGO_new$manyID == "CAN-NOR[CSS]_1975N")
# Two fully duplicated treaties, remove duplicated rows
remove <- which(HUGGO_new$manyID == "CAN-NOR[CSS]_1975N" & HUGGO_new$Force == "1975-04-23")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "CAN-NOR[CSS]_1975N" & HUGGO_new$Force == "1975-12-12")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# GU14US_1975A
which(HUGGO_new$manyID == "GU14US_1975A")
# Two fully duplicated treaties, remove duplicated rows
remove <- which(HUGGO_new$manyID == "GU14US_1975A" & HUGGO_new$Signature == "1975-05-30")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "GU14US_1975A" & HUGGO_new$Signature == "1975-12-16")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# GUY-RUS[UNF]_1977A 
which(HUGGO_new$manyID == "GUY-RUS[UNF]_1977A")
# Fully duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "GUY-RUS[UNF]_1977A")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# DEU-LUX[FFA]_1980N
which(HUGGO_new$manyID == "DEU-LUX[FFA]_1980N")
# Duplicate rows, keep one with more data
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "DEU-LUX[FFA]_1980N" & is.na(HUGGO_new$Ambit))), ]

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
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "TPNWLA_1991E:TPNWLA_1967A" & is.na(HUGGO_new$url))), ]

# SWZ-ZAF[DUK]_1992A
which(HUGGO_new$manyID == "SWZ-ZAF[DUK]_1992A")
# Fully duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "SWZ-ZAF[DUK]_1992A")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# RUS-UKR[FSA]_1993A
which(HUGGO_new$manyID == "RUS-UKR[FSA]_1993A")
# Duplicate rows, keep the one with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "RUS-UKR[FSA]_1993A" & is.na(HUGGO_new$url))), ]

# EB06DB_1993A
which(HUGGO_new$manyID == "EB06DB_1993A")
# Fully duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "EB06DB_1993A")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# BJ06TP_1995A
# Two fully duplicated treaties
# Jordan and Sudan
remove <- which(HUGGO_new$manyID == "BJ06TP_1995A" & HUGGO_new$Beg == "1995-02-19")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
# Jordan and Tunis
remove <- which(HUGGO_new$manyID == "BJ06TP_1995A" & HUGGO_new$Beg == "1995-04-27")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# CTMHWD_1995E:CTMHWD_1989A
which(HUGGO_new$manyID == "CTMHWD_1995E:CTMHWD_1989A")
# Duplicate rows, keep one with verified metadata
HUGGO_new <- HUGGO_new[-(which(HUGGO_new$manyID == "CTMHWD_1995E:CTMHWD_1989A" & is.na(HUGGO_new$url))), ]

# GIN-EC[ECF]_1995P
which(HUGGO_new$manyID == "GIN-EC[ECF]_1995P")
# Fully duplicate rows, keep one
remove <- which(HUGGO_new$manyID == "GIN-EC[ECF]_1995P")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# CE06NS_1999A
which(HUGGO_new$manyID == "CE06NS_1999A")
# Two fully duplicated treaties
# Remove duplicate rows
remove <- which(HUGGO_new$manyID == "CE06NS_1999A" & HUGGO_new$Beg == "1999-07-19")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "CE06NS_1999A" & HUGGO_new$Beg == "1999-07-23")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# AGO-EC[CPF]_2000P
which(HUGGO_new$manyID == "AGO-EC[CPF]_2000P")
# Two fully duplicated treaties, remove duplicate rows
remove <- which(HUGGO_new$manyID == "AGO-EC[CPF]_2000P" & HUGGO_new$Beg == "2000-03-30")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "AGO-EC[CPF]_2000P" & HUGGO_new$Beg == "2000-07-06")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]

# SEN-EC[CFP]_2002A
which(HUGGO_new$manyID == "SEN-EC[CFP]_2002A")
# Two fully duplicated treaties, remove duplicate rows
remove <- which(HUGGO_new$manyID == "SEN-EC[CFP]_2002A" & HUGGO_new$End == "2001-12-31")
HUGGO_new <- HUGGO_new[-(remove[-1]), ]
remove <- which(HUGGO_new$manyID == "SEN-EC[CFP]_2002A" & HUGGO_new$End == "2001-07-31")
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

# Standardise date columns, arrange by Beg, and push HUGGO_new to HUGGO
HUGGO <- HUGGO_new %>%
              dplyr::mutate(Beg = messydates::as_messydate(Beg),
              Signature = messydates::as_messydate(Signature),
              Force = messydates::as_messydate(Force),
              End = messydates::as_messydate(End)) %>%
              dplyr::arrange(Beg)
              

# Arrange by Beg date
HUGGO_new <- arrange(HUGGO_new)

# Step three: Push HUGGO_new to HUGGO
HUGGO <- HUGGO_new

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
