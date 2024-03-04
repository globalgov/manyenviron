# ECOLEX_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many package.

# Stage one: Collecting data
ECOLEX_MEM <- load("data-raw/memberships/ECOLEX_MEM/ecomembs.RData")
ECOLEX_MEM <- eco_membs
manypkgs::retain("ECOLEX_MEM")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'ECOLEX_MEM' object until the object created
# below (in stage three) passes all the tests.
library(dplyr)
ECOLEX_MEM <- as_tibble(ECOLEX_MEM) %>%
  dplyr::rename(For = Force,
                Rati = Rat) %>%
  manydata::transmutate(stateSignature = messydates::as_messydate(Sign),
                        End = messydates::as_messydate(Term),
                        Force = messydates::as_messydate(For),
                        Rat = messydates::as_messydate(Rati),
                        stateID = StatID,
                        ecolexID = EcolexID) %>%
  dplyr::mutate(Begin = dplyr::coalesce(stateSignature, Rat, Force)) %>%
  # Check Signature date is for the country and not document signature date
  dplyr::select(stateID, Begin, End, stateSignature, Force, Rat, ecolexID) %>%
  dplyr::arrange(Begin)

# Add a Title column
ECOLEX <- manyenviron::agreements$ECOLEX %>%
  dplyr::select(Title, ecolexID)
ECOLEX_MEM <- dplyr::left_join(ECOLEX_MEM, ECOLEX, by = "ecolexID")

#Add a treatyID column
ECOLEX_MEM$treatyID <- manypkgs::code_agreements(ECOLEX_MEM,
                                                 ECOLEX_MEM$Title,
                                                 ECOLEX_MEM$Begin)

# Add manyID column
manyID <- manypkgs::condense_agreements(manyenviron::memberships)
ECOLEX_MEM <- dplyr::left_join(ECOLEX_MEM, manyID, by = "treatyID")

# Re-order the columns
ECOLEX_MEM <- dplyr::relocate(ECOLEX_MEM,
                              c("manyID", "stateID", "Title"))

# manypkgs includes several functions that should help
# cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make ECOLEX_MEM available
# within the package.
manypkgs::export_data(ECOLEX_MEM, datacube = "memberships",
                      URL = "https://www.ecolex.org/result/?type=treaty")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure
# adherence to certain standards. You can hit Cmd-Shift-T (Mac)
# or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill
#in as much detail about the variables etc as possible.
