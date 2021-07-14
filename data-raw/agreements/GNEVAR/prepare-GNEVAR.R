# GNEVAR Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
library(qCreate)

# Stage one: Collecting data
GNEVAR <- readr::read_csv("data-raw/agreements/GNEVAR/EnvGov Nodes-Table 1 VERS2.csv")
GNEVAR2 <- readr::read_csv2("data-raw/agreements/GNEVAR/endb4beg.csv")
GNEVAR3 <- readr::read_csv("data-raw/agreements/GNEVAR/gnevar.csv")
GNEVAR4 <- readr::read_csv("data-raw/agreements/GNEVAR/GENG v1.2 (31.10.2015).csv")
GNEVAR5 <- readr::read_csv2("data-raw/agreements/GNEVAR/duplicates v1.0.csv")

GNEVAR <- link_metadata(GNEVAR)
# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'GNEVAR' object until the object created
# below (in stage three) passes all the tests. 
GNEVAR <- as_tibble(GNEVAR)  %>%
  tidyr::separate(IEA, c("NEW", "IEADB_ID"), sep = "-") %>% 
  dplyr::mutate(D=dplyr::recode(T, G="A", M="E", "T"="Q", D="V", R="W", N="X", U="Y")) %>% 
  transmutate(Signature = messydates::as_messydate(DocSign),
              End = messydates::as_messydate(DocEnd),
              Force = messydates::as_messydate(DocForce),# some dates formats are failing to pass (e.i 0000-00-00)
              GNEVAR_ID = GENG,
              ECOLEX_ID = ECOLEX) %>% 
  dplyr::mutate(Title = standardise_titles(Title)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>% 
  dplyr::select(GNEVAR_ID, Title, Beg, End, L,J,D, Signature, Force) %>% 
  dplyr::arrange(Beg)

GNEVAR$qID<- qCreate::code_agreements(GNEVAR, GNEVAR$Title, GNEVAR$Beg)

# # Clean GNEVAR 3
# GNEVAR3 <- as_tibble(GNEVAR3) %>% 
#   transmutate()
# 
# # Clean GNEVAR 4
# GNEVAR4$Parties <- paste0(GNEVAR4$Country.x, "-", GNEVAR4$Country.y)
# GNEVAR4 <- as_tibble(GNEVAR4) %>% 
#   transmutate(Signature = standardise_dates(DocDate),
#               Force = standardise_dates(InForce)) %>% 
#   dplyr::mutate(End = standardise_dates(End)) %>% 
#   dplyr::mutate(Title = standardise_titles(Title)) %>% 
#   dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>% 
#   dplyr::select(Title, Beg, Signature, Force, End, Parties)

# qData includes several functions that should help cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make GNEVAR available within the qPackage.
export_data(GNEVAR, database = "agreements", URL = "NA")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible