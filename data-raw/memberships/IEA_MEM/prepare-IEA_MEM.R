# IEA_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
library(qData)

# Stage one: Collecting data
IEA_MEM <- readxl::read_excel("data-raw/memberships/IEA_MEM/iea-memb.xlsx")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'IEA_MEM' object until the object created
# below (in stage three) passes all the tests. 
IEA_MEM <- as_tibble(IEA_MEM) %>%
  transmutate(ID = standardise_titles(country),
              Title = standardise_titles(treatyname),
              Signature = `tsig`,
              Beg = `csig`,
              Rat = `crat`,
              End = `tterm`,
              Force = `ceif3`) %>%
  dplyr::select(ID, Title, Signature, Beg, Rat, Force, End) %>% 
  dplyr::arrange(Signature, ID)

# qData includes several functions that should help cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IEA_MEM available within the qPackage.
export_data(IEA_MEM, database = "memberships")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible.
