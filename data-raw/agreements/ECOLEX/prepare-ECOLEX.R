# ECOLEX Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many packages universe.

# Stage one: Collecting data
load("data-raw/agreements/ECOLEX/ecoagree.RData")
ECOLEX <- eco_agree
manypkgs::retain("ECOLEX")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'ECOLEX' object until the object created
# below (in stage three) passes all the tests. 
ECOLEX <- as_tibble(ECOLEX) %>% 
  dplyr::rename("title" = "Title") %>% 
  dplyr::mutate(L = dplyr::recode(Document.type, "Bilateral" = "B", "Multilateral" = "M")) %>% 
  dplyr::mutate(J = dplyr::recode(Field.of.application, "Global" = "G", "Regional/restricted" = "R")) %>% 
  qData::transmutate(ECOLEX_ID = `EcolexID`,
                     Title = manypkgs::standardise_titles(title), #Key API has been used here
                     # to translate treaties to English
                     Signature = manypkgs::standardise_dates(lubridate::mdy(Date)),
                     Force = manypkgs::standardise_dates(lubridate::mdy(`Entry.into.force`))) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>% 
  dplyr::select(ECOLEX_ID, Title, Beg, L, J, Signature, Force) %>% 
  dplyr::arrange(Beg)

# Add treaty_ID column
ECOLEX$treaty_ID <- manypkgs::code_agreements(ECOLEX, ECOLEX$Title, ECOLEX$Beg)

# Add many_ID column
many_ID <- manypkgs::condense_agreements(manyenviron::agreements)
ECOLEX <- dplyr::left_join(ECOLEX, many_ID, by = "treaty_ID")

# Re-order the columns
ECOLEX <- ECOLEX %>% 
  dplyr::select(many_ID, Title, Beg, L, J, Signature, Force, treaty_ID, ECOLEX_ID) %>% 
  dplyr::arrange(Beg)

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make ECOLEX available
# within the package.
manypkgs::export_data(ECOLEX, database = "agreements", URL = "https://www.ecolex.org/result/?type=treaty")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible.
