# TFDD_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for many packages universe.

# Stage one: Collecting data
TFDD_MEM <- readxl::read_excel("data-raw/memberships/TFDD_MEM/TFDD.xlsx")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'TFDD_MEM' object until the object created
# below (in stage three) passes all the tests.
TFDD_MEM <- as_tibble(TFDD_MEM) %>%
  dplyr::mutate(Signature = standardise_dates(openxlsx::convertToDate(DateSigned))) %>% 
  dplyr::mutate(Beg = manypkgs::standardise_dates(as.character(Signature))) %>%
  qData::transmutate(TFDD_ID = `2016Update ID`,
                     CountryID = CCODE,
                     Title = manypkgs::standardise_titles(DocumentName)) %>% 
  # API has been used to translate some of the treaties that were not in english
  dplyr::mutate(Memberships = qStates::code_states(Signatories)) %>% 
  dplyr::select(CountryID, Title, Beg, Signature, TFDD_ID, Memberships) %>% 
  dplyr::arrange(Beg)

# Add a qID column
TFDD_MEM$qID <- manypkgs::code_agreements(TFDD_MEM, TFDD_MEM$Title, TFDD_MEM$Beg)

# Add qID_ref column
qID_ref <- manypkgs::condense_qID(manyenviron::memberships)
TFDD_MEM <- dplyr::left_join(TFDD_MEM, qID_ref, by = "qID")

# Re-order the columns
TFDD_MEM <- dplyr::relocate(TFDD_MEM, qID_ref)

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make TFDD_MEM available within the package.
manypkgs::export_data(TFDD_MEM, database = "memberships", URL = "https://transboundarywaters.science.oregonstate.edu/")
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
