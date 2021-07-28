# IEADB Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
library(qCreate)
library(qData)

# Stage one: Collecting data
IEADB <- readr::read_delim("data-raw/agreements/IEADB/treaties.csv", ",")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'IEADB' object until the object created
# below (in stage three) passes all the tests. 
IEADB <- as_tibble(IEADB)  %>%
  dplyr::mutate(D = dplyr::recode(`Agreement Type (level 2)`, "Agreement" = "A", "Amendment" = "E",
                                  "Agreed Minute (non-binding)" ="Q", "Declaration" = "V",
                                  "Resolution" = "W", "Exchange of Notes" = "X",
                                  "Memorandum of Understanding" = "Y", "Protocol" = "P")) %>% 
  dplyr::mutate(L = dplyr::recode(Inclusion, "BEA" = "B", "MEA" = "M")) %>% 
  dplyr::filter(L == "M" | L == "B") %>%
  transmutate(IEADB_ID = as.character(`IEA# (click for add'l info)`),
              Title = standardise_titles(`Treaty Name`),
              Signature = messydates::as_messydate(`Signature Date`),
              Force = messydates::as_messydate(`Date IEA entered into force`)) %>% 
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>% 
  dplyr::select(IEADB_ID, Title, Beg, L, D, Signature, Force) %>% 
  dplyr::arrange(Beg)

# Add qID column
IEADB$qID <- qCreate::code_agreements(IEADB, IEADB$Title, IEADB$Beg)

# qData includes several functions that should help cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IEADB available within the qPackage.
export_data(IEADB, database = "agreements", URL = "https://iea.uoregon.edu/base-agreement-list")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible.