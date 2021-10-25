# ENVIRONMENTAL AGREEMENTS TEXTS script preparation

# Due to the specificities of the text database, 
# the usual script preparation format has been adapted.

# Step one: create a consolidated version of qEnviron agreements database
agreements <- qData::consolidate(qEnviron::agreements, "any", "any")

# Step two: extract treaty texts from IEADB website
IEADB_original <- readr::read_csv("data-raw/agreements/IEADB/treaties.csv")
IEADB_original <- dplyr::as_tibble(IEADB_original) %>%
  dplyr::rename(IEADB_ID = `IEA# (click for add'l info)`,
                TreatyText = `Treaty Text`) %>% 
  dplyr::select(IEADB_ID, TreatyText) %>% 
  dplyr::filter(TreatyText == "Treaty Text**") %>% 
  dplyr::arrange(IEADB_ID)
IEADB_original$IEADB_ID <- as.character(IEADB_original$IEADB_ID)


base <- "https://iea.uoregon.edu/treaty-text/"
IEADB_original$TreatyText <- lapply(IEADB_original$IEADB_ID, function(s) tryCatch(rvest::read_html(paste0(base, s)) %>%
                                                                                    rvest::html_nodes(".even") %>% 
                                                                                    rvest::html_text(),error = function(e){as.character("Not found")} %>% 
                                                                                    paste(collapse = ",")))

IEADB_original$Source <- lapply(IEADB_original$IEADB_ID, function(s) tryCatch(rvest::read_html(paste0(base, s)) %>%
                                                                                rvest::html_nodes(".views-field-views-conditional") %>% 
                                                                                rvest::html_text(),error = function(e){as.character("Not found")} %>% 
                                                                                paste(collapse = ",")))

# Step three: join IEADB text column 
agreements <- dplyr::left_join(agreements, IEADB_original, by = "IEADB_ID")
agreements <- as_tibble(agreements) %>% 
  dplyr::select(qID, IEADB_ID, GNEVAR_ID, ECOLEX_ID, qID_ref, Title, Beg, TreatyText, Source)

# Step four: complement the dataset with ECOLEX treaty texts (TO BE IMPROVED)
ecolex_text <- agreements %>% 
  dplyr::filter(!is.na(ECOLEX_ID) & TreatyText == "NULL")

ecolex_text$ECOLEX_ID <- stringr::str_remove_all(ecolex_text$ECOLEX_ID, "-")

base = "http://www.ecolex.org/server2neu.php/libcat/docs/TRE/Full/En/"
ecolex_text$TreatyText <- lapply(ecolex_text$ECOLEX_ID, function(s) tryCatch(pdftools::pdf_text(paste0(base, s, ".pdf")), error = function(e){as.character("Not found")}))

ecolex_text$Source <- lapply(ecolex_text$ECOLEX_ID, function(s) paste0(base, s, ".pdf"))

ecolex_text <- ecolex_text %>% 
  dplyr::filter(TreatyText != "Not found") %>% 
  dplyr::select(qID_ref, TreatyText)

# Step five: join ECOLEX texts to the agreements dataset
agreements <- dplyr::left_join(agreements,  ecolex_text, by = "qID_ref")

AGR_TXT <- dplyr::as_tibble(agreements)
  
# Step six: export data into rda format 2 possible solutions
#qCreate::export_data(AGR_TXT, database = "texts", URL = "NA")
# usethis::use_data(AGR_TXT, internal = T, overwrite = T)

