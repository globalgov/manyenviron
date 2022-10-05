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
                                                   accession = "process")

# Remove duplicates and convert NAs
AIGGO <- AIGGO %>%
  dplyr::relocate(manyID, Title, Beg, End, DocType, AgreementType, GeogArea,
                  Signature, Force, Lineage, accessionC, accessionP) %>%
  manydata::transmutate(SourceText = as.character(Source)) %>%
  dplyr::mutate(accessionC = gsub("NA", NA, accessionC)) %>%
  dplyr::mutate(accessionP = gsub("NA", NA, accessionP)) %>%
  dplyr::arrange(Beg)

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
