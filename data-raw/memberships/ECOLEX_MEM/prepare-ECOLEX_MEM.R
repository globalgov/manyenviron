# ECOLEX_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
library(qData)

# Stage one: Collecting data
ECOLEX_MEM <- load("data-raw/memberships/ECOLEX_MEM/ecomembs.RData")
ECOLEX_MEM <- eco_membs
retain(c("ECOLEX_MEM"))

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'ECOLEX_MEM' object until the object created
# below (in stage three) passes all the tests. 
ECOLEX_MEM <- as_tibble(ECOLEX_MEM) %>%
  dplyr::rename(For = Force,
                Rati = Rat,
                ECOLEX_ID = EcolexID,
                ID = StatID) %>% 
  transmutate(Beg = standardise_dates(Sign),
              End = standardise_dates(Term),
              Force = standardise_dates(For),
              Rat = standardise_dates(Rati)) %>%
  dplyr::select(ID, ECOLEX_ID, Beg, End, Force, Rat) %>% 
  dplyr::mutate(Beg = coalesce(Beg, Rat, Force)) %>% 
  dplyr::select(ID, ECOLEX_ID, Beg, End)
# qData includes several functions that should help cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make ECOLEX_MEM available within the qPackage.
export_data(ECOLEX_MEM, database = "memberships", link = "https://www.ecolex.org/result/?type=treaty")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible.