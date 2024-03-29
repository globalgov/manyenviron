# CIESIN Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many packages universe.

# Stage one: Collecting data
CIESIN <- readxl::read_excel("data-raw/agreements/CIESIN/CIESIN.xls")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'CIESIN' object until the object created
# below (in stage three) passes all the tests.
CIESIN <- as_tibble(CIESIN) %>%
  manydata::transmutate(Title = manypkgs::standardise_titles(`Treaty Title`,
                                                             api_key = api),
                        # Define Key API
                     Signature = messydates::as_messydate(`Year of Agreement`),
                     Force = messydates::as_messydate(`Year of Entry into Force`)) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::select(Title, Beg, Signature, Force) %>%
  dplyr::arrange(Beg)

# Add treatyID column
CIESIN$treatyID <- manypkgs::code_agreements(CIESIN,
                                             CIESIN$Title,
                                             CIESIN$Beg)
#Add Lineage column
CIESIN$Lineage <- manypkgs::code_lineage(CIESIN$Title)

# Add manyID column
manyID <- manypkgs::condense_agreements(manyenviron::agreements)
CIESIN <- dplyr::left_join(CIESIN, manyID, by = "treatyID")

# Re-order the columns
CIESIN <- CIESIN %>%
  dplyr::select(manyID, Title, Beg, Signature,
                Force, Lineage, treatyID) %>%
  dplyr::arrange(Beg)

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make CIESIN available
# within the package.
manypkgs::export_data(CIESIN,
                      database = "agreements",
                      URL = "https://sedac.ciesin.columbia.edu/entri/")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure
# adherence to certain standards. You can hit Cmd-Shift-T (Mac)
# or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill
# in as much detail about the variables etc as possible.
