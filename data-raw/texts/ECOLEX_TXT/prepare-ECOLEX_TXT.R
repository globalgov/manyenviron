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
ECOLEX_TXT <- ECOLEX_TXT %>% 
  dplyr::mutate(Title = qCreate::standardise_titles(Title))
  dplyr::select(EcolexID, `Full.text`) %>% 
  dplyr::mutate(TreatyText = ifelse(stringr::str_detect(ECOLEX_TXT$Full.text, "English"), "English", NA))

ECOLEX_TXT$EcolexID <- stringr::str_remove_all(ECOLEX_TXT$EcolexID, "-")

ECOLEX_TXT <- ECOLEX_TXT %>% 
  dplyr::filter(TreatyText == "English")
ECOLEX_TXT <- as_tibble(ECOLEX_TXT) %>%
  qData::transmutate(ID = {id_variable_name_here},
              Beg = qCreate::standardise_dates({date_variable_name_here})) %>%
  dplyr::arrange(Beg)
# qCreate includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make ECOLEX_TXT available
# within the qPackage.
qCreate::export_data(ECOLEX_TXT, database = "texts")
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
