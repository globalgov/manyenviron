# ECOLEX_TXT Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.

# Stage one: Collecting data
load("data-raw/texts/ECOLEX_TXT/ecoagree.RData")
ECOLEX_TXT <- eco_agree
qCreate::retain("ECOLEX_TXT")
# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'ECOLEX_TXT' object until the object created
# below (in stage three) passes all the tests.
# First step: standardise, select and add useful variables
ECOLEX_TXT <- as_tibble(ECOLEX_TXT) %>% 
  dplyr::mutate(Title = qCreate::standardise_titles(Title),
                Beg = qCreate::standardise_dates(lubridate::mdy(Date)),
                ID = EcolexID,
                TreatyText = ifelse(stringr::str_detect(ECOLEX_TXT$Full.text, "English"), "English", NA)) %>% 
  dplyr::select(ID, Title, Beg, TreatyText)

ECOLEX_TXT$ID <- stringr::str_remove_all(ECOLEX_TXT$ID, "-")

ECOLEX_TXT$qID <- qCreate::code_agreements(ECOLEX_TXT, ECOLEX_TXT$Title, ECOLEX_TXT$Beg)

# Step two: extract treaty texts from ecolex website (only the ones in english for now)
ECOLEX_TXT1 <- ECOLEX_TXT %>% 
  dplyr::filter(TreatyText == "English")

base = "http://www.ecolex.org/server2neu.php/libcat/docs/TRE/Full/En/"
ECOLEX_TXT1$Text <- lapply(ECOLEX_TXT1$ID, function(s) tryCatch(pdftools::pdf_text(paste0(base, s, ".pdf")), error = function(e){as.character("Not found")}))
ECOLEX_TXT1 <- ECOLEX_TXT1 %>% 
  dplyr::select(ID, Text)

ECOLEX_TXT <- left_join(ECOLEX_TXT, ECOLEX_TXT1, by = "ID")
# qCreate includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make ECOLEX_TXT available
# within the qPackage.
qCreate::export_data(ECOLEX_TXT, database = "texts", URL = "https://www.ecolex.org/result/?type=treaty")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence
# to certain standards.You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows)
# to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please note that the export_data() function requires a .bib file to be
# present in the data_raw folder of the package for citation purposes.
# Therefore, please make sure that you have permission to use the dataset
# that you're including in the package.
