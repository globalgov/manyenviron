# HEIDI Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many package.

# Stage one: Collecting data
HEIDI <- readxl::read_excel("data-raw/agreements/HEIDI/heidi_dataset.xlsx")
HEIDI$signature.date <- openxlsx::convertToDate(HEIDI$signature.date)

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'HEIDI' object until the object created
# below (in stage three) passes all the tests.
HEIDI <- as_tibble(HEIDI) %>%
  manydata::transmutate(Title = manypkgs::standardise_titles(`Name.of.the.agreement`),
                        Signature = messydates::as_messydate(`signature.date`)) %>%
  dplyr::mutate(Begin = Signature) %>%
  dplyr::rename(heidiID = ID) %>%
  dplyr::select(heidiID, Title, Begin, Signature) %>%
  dplyr::arrange(Begin)

# Add treaty_ID column
HEIDI$treatyID <- manypkgs::code_agreements(HEIDI, HEIDI$Title, HEIDI$Begin)

# Add Lineage column
HEIDI$Lineage <- manypkgs::code_lineage(HEIDI$Title)

# Add many_ID column
manyID <- manypkgs::condense_agreements(manyenviron::agreements)
HEIDI <- dplyr::left_join(HEIDI, manyID, by = "treatyID")

# Re-order the columns
HEIDI <- HEIDI %>%
  dplyr::select(manyID, Title, Begin, Signature,
                Lineage, treatyID, heidiID) %>%
  dplyr::arrange(Begin)

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IEADB available
# within the package.
manypkgs::export_data(HEIDI, datacube = "agreements",
                      URL = "https://www.chaire-epi.ulaval.ca/en/data/heidi")
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
