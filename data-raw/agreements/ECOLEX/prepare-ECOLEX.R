# ECOLEX Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
library(qCreate)
library(qData)

# Stage one: Collecting data
load("data-raw/agreements/ECOLEX/ecoagree.RData")
ECOLEX <- eco_agree
retain("ECOLEX")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'ECOLEX' object until the object created
# below (in stage three) passes all the tests. 
ECOLEX <- as_tibble(ECOLEX) %>% 
  dplyr::rename("title" = "Title") %>% 
  dplyr::mutate(L = dplyr::recode(Document.type, "Bilateral" = "B", "Multilateral" = "M")) %>% 
  dplyr::mutate(J = dplyr::recode(Field.of.application, "Global" = "G", "Regional/restricted" = "R")) %>% 
  transmutate(ECOLEX_ID = `EcolexID`,
              Title = standardise_titles(title),
              Signature = messydates::as_messydate(lubridate::mdy(Date)), 
              Force = messydates::as_messydate(lubridate::mdy(`Entry.into.force`))) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>% 
  dplyr::select(ECOLEX_ID, Title, Beg, L, J, Signature, Force) %>% 
  dplyr::arrange(Beg)

# Add qID column
ECOLEX$qID <- qCreate::code_agreements(ECOLEX, ECOLEX$Title, ECOLEX$Beg)

# qData includes several functions that should help cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make ECOLEX available within the qPackage.
export_data(ECOLEX, database = "agreements", URL = "https://www.ecolex.org/result/?type=treaty")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible.
