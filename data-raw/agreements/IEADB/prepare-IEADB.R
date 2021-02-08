# IEADB Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.
library(qData)
library(tidyverse)

# Stage one: Collecting data
IEADB <- read_delim("data-raw/agreements/IEADB/treaties.csv", ",")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'IEADB' object until the object created
# below (in stage three) passes all the tests. 
IEADB <- as_tibble(IEADB)  %>% 
  filter(Inclusion == "MEA" | Inclusion == "BEA") %>%
  transmutate(IEADB_ID = `IEA# (click for add'l info)`,
              Title = `Treaty Name`,
              Beg = standardise_dates(`Signature Date`),
              Force = standardise_dates(`Date IEA entered into force`)) %>% 
  dplyr::select(IEADB_ID, Title, Beg) %>% 
  arrange(Beg)

# qData includes several functions that should help cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IEADB available within the qPackage.
export_data(IEADB, database = "agreements", link = "https://iea.uoregon.edu/base-agreement-list")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible.