# AIGGO Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many packages universe.

# Start with HUGGO dataset
AIGGO <- manyenviron::agreements$HUGGO

# Add Lineage Column
AIGGO$Lineage <- manypkgs::code_lineage(AIGGO$Title)

# Code accession conditions and procedures
AIGGO$accessionC <- manypkgs::code_accession_terms(AIGGO$TreatyText,
                                                   AIGGO$Title,
                                                   accession = "condition")
AIGGO$accessionP <- manypkgs::code_accession_terms(AIGGO$TreatyText,
                                                   AIGGO$Title,
                                                   accession = "process")
# Code termination clause and date
AIGGO$termination_type <- manypkgs::code_term(AIGGO$Title, AIGGO$TreatyText)

AIGGO$termination_date <- manypkgs::code_term_date(AIGGO$Title,
                                                   AIGGO$TreatyText)
# Remove duplicates and convert NAs
AIGGO <- AIGGO %>%
  dplyr::select(manyID, Lineage, accessionC, accessionP,
                  termination_type, termination_date) %>%
  dplyr::mutate(accessionC = gsub("NA", NA, accessionC),
                accessionP = gsub("NA", NA, accessionP))

# Stage three: Connecting data
manypkgs::export_data(AIGGO, database = "agreements",
                      URL = "Programatically coded data by the GGO team")

# This function also does two additional things.
# First, it creates a set of tests for this object to ensure
# adherence to certain standards. You can hit Cmd-Shift-T (Mac)
# or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and
# fill in as much detail about the variables etc as possible.
