# PDVD Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for many packages universe.

# Stage one: Collecting data
PDVD <- readr::read_delim("data-raw/organizations/PDVD/PDVD_v3_votes (4).csv", ";")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'PDVD' object until the object created
# below (in stage three) passes all the tests.
# PDVD <- as_tibble(PDVD) %>%
#   manydata::transmutate(ID = {id_variable_name_here},
#               Beg = manypkgs::standardise_dates({date_variable_name_here})) %>%
#   dplyr::arrange(Beg)
# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make PDVD available
# within the package.
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence
# to certain standards.You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows)
# to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please note that the export_data() function requires a .bib file to be
# present in the data_raw folder of the package for citation purposes.
# Therefore, please make sure that you have permission to use the dataset
# that you're including in the package.
# To add a template of .bib file to package,
# run `manypkgs::add_bib(organizations, PDVD)`.
manypkgs::export_data(PDVD, database = "organizations",
                     URL = NULL)
