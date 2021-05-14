# IEA_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
library(qCreate)

# Stage one: Collecting data
IEA_MEM <- readxl::read_excel("data-raw/memberships/IEA_MEM/iea-memb.xlsx")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'IEA_MEM' object until the object created
# below (in stage three) passes all the tests. 
IEA_MEM <- as_tibble(IEA_MEM) %>%
  dplyr::rename(IEADB_ID = mitch_id) %>% 
  transmutate(Country = standardise_titles(country),
              Title = standardise_titles(treatyname),
              Signature = standardise_dates(tsig),
              SignatureC = standardise_dates(csig),
              Rat = standardise_dates(crat),
              End = standardise_dates(tterm),
              Force = standardise_dates(ceif3),
              Force2 = standardise_dates(ceif4)) %>%
  dplyr::select(IEADB_ID, Country, Title, Signature, End, Rat, Force, Force2, SignatureC) %>% 
  tidyr::pivot_longer(c(Force2, Force), values_to = "Force")

IEA_MEM <- IEA_MEM[!(is.na(IEA_MEM$Force) & IEA_MEM$name =="Force2"),] %>% 
  dplyr::mutate(Beg = dplyr::coalesce(SignatureC, Rat, Force)) %>% 
  dplyr::select(IEADB_ID, Country, Title, Beg, End, SignatureC, Signature, Rat, Force) %>% 
  dplyr::arrange(Beg)
# qData includes several functions that should help cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IEA_MEM available within the qPackage.
export_data(IEA_MEM, database = "memberships", URL = "https://iea.uoregon.edu/country-members")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible.