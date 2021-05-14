# GNEVAR Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
library(qCreate)

# Stage one: Collecting data
GNEVAR <- readr::read_csv("data-raw/agreements/GNEVAR/EnvGov Nodes-Table 1 VERS2.csv")
GNEVAR2 <- readr::read_csv2("data-raw/agreements/GNEVAR/endb4beg.csv")
# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'GNEVAR' object until the object created
# below (in stage three) passes all the tests. 
GNEVAR <- as_tibble(GNEVAR)  %>%
  tidyr::separate(IEA, c("NEW", "IEADB_ID"), sep = "-") %>% 
  dplyr::mutate(D=dplyr::recode(T, G="A", M="E", "T"="Q", D="V", R="W", N="X", U="Y")) %>% 
  transmutate(Signature = standardise_dates(DocSign),
              End = standardise_dates(DocEnd),
              Force = standardise_dates(DocForce),# some dates formats are failing to pass (e.i 0000-00-00)
              GNEVAR_ID = GENG,
              ECOLEX_ID = ECOLEX) %>% 
  dplyr::mutate(Title = standardise_titles(Title)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>% 
  dplyr::select(GNEVAR_ID, Title, Beg, End, L,J,D, Signature, Force) %>% 
  dplyr::arrange(Signature)

GNEVAR$qID<- qCreate::code_agreements(GNEVAR$Title, GNEVAR$Beg)

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