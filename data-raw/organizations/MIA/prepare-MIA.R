# MIA Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
library(qData)

# Stage one: Collecting data
MIA <- haven::read_dta("data-raw/international organizations/MIA/DP_volIII_june2019_15.dta")

MIA <- link_metadata(MIA)

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'MIA' object until the object created
# below (in stage three) passes all the tests.
MIA <- as_tibble(MIA) %>%
  dplyr::group_by(ioname) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(Name = ioname,
                COW = ionumber) %>% 
  transmutate(Beg = standardise_dates(inception)) %>%
  dplyr::select(Name, Beg, COW) %>% 
  dplyr::arrange(Beg)
# qData includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make MIA available 
# within the qPackage.
export_data(MIA, database = "international organizations")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence
# to certain standards.You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows)
# to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as
# much detail about the variables etc as possible.
