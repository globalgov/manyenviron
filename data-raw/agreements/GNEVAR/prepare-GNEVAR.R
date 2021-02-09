# GNEVAR Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
library(qData)

# Stage one: Collecting data
GNEVAR <- readr::read_csv("data-raw/agreements/GNEVAR/EnvGov Nodes-Table 1 VERS2.csv")
# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'GNEVAR' object until the object created
# below (in stage three) passes all the tests. 
GNEVAR <- as_tibble(GNEVAR)  %>% 
  transmutate(Beg = standardise_dates(DocSign),
              End = standardise_dates(DocEnd),
              Force = standardise_dates(DocForce),# some dates formats are failing to pass (e.i 0000-00-00)
              ID = GENG) %>%
  dplyr::select(ID, Title, Beg, End) %>% 
  dplyr::arrange(Beg, ID)

# qData includes several functions that should help cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make GNEVAR available within the qPackage.
export_data(GNEVAR, database = "agreements", link = "https://github.com/jhollway/gnevar")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible