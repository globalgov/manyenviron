# HEIDI Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for many packages universe.

# Stage one: Collecting data
HEIDI <- readxl::read_excel("data-raw/agreements/HEIDI/heidi_dataset.xlsx")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'HEIDI' object until the object created
# below (in stage three) passes all the tests.
HEIDI <- as_tibble(HEIDI) %>%
  manydata::transmutate(Title = manypkgs::standardise_titles(`Name.of.the.agreement`),
                        Signature = manypkgs::standardise_dates(`signature.date`)) %>%#has to change the dates format
  dplyr::mutate(Beg = Signature) %>%
  dplyr::rename(HEIDI_ID = ID) %>% 
  dplyr::select(HEIDI_ID, Title, Beg, Signature) %>%
  dplyr::arrange(Beg)

# Add treaty_ID column
HEIDI$treaty_ID <- manypkgs::code_agreements(HEIDI, HEIDI$Title, HEIDI$Beg)

# Add many_ID column
many_ID <- manypkgs::condense_agreements(manyenviron::agreements)
HEIDI <- dplyr::left_join(HEIDI, many_ID, by = "treaty_ID")

# Re-order the columns
HEIDI <- HEIDI %>% 
  dplyr::select(many_ID, Title, Beg, Signature, treaty_ID, HEIDI_ID) %>% 
  dplyr::arrange(Beg)

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make HEIDI available
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
# run `manypkgs::add_bib(agreements, HEIDI)`.
manypkgs::export_data(HEIDI, database = "agreements",
                      URL = "https://www.chaire-epi.ulaval.ca/en/data/heidi")

