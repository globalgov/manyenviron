# ECOLEX Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many packages universe.

# Stage one: Collecting data
load("data-raw/agreements/ECOLEX/ecoagree.RData")
ECOLEX <- eco_agree
manypkgs::retain("ECOLEX")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'ECOLEX' object until the object created
# below (in stage three) passes all the tests.
ECOLEX <- as_tibble(ECOLEX) %>%
  dplyr::rename("title" = "Title") %>%
  dplyr::mutate(DocType = dplyr::recode(Document.type,
                                  "Bilateral" = "B",
                                  "Multilateral" = "M")) %>%
  dplyr::mutate(GeogArea = dplyr::recode(Field.of.application,
                                  "Global" = "G",
                                  "Regional/restricted" = "R")) %>%
  manydata::transmutate(ecolexID = `EcolexID`,
                     Title = manypkgs::standardise_titles(title,
                                                          api_key = api),
                     # Define Key API
                     Signature = manypkgs::standardise_dates(lubridate::mdy(Date)),
                     Force = manypkgs::standardise_dates(lubridate::mdy(`Entry.into.force`))) %>%
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::select(ecolexID, Title, Beg, DocType, GeogArea, Signature, Force) %>%
  dplyr::arrange(Beg)

# Add treatyID column
ECOLEX$treatyID <- manypkgs::code_agreements(ECOLEX,
                                             ECOLEX$Title,
                                             ECOLEX$Beg)
# Add Lineage column
ECOLEX$Lineage <- manypkgs::code_lineage(ECOLEX$Title)

# Add manyID column
manyID <- manypkgs::condense_agreements(manyenviron::agreements)
ECOLEX <- dplyr::left_join(ECOLEX, manyID, by = "treatyID")

# Re-order the columns
ECOLEX <- ECOLEX %>% 
  dplyr::select(manyID, Title, Beg, DocType, GeogArea, Signature,
                Force, Lineage, treatyID, ecolexID) %>%
  dplyr::arrange(Beg)

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make ECOLEX available
# within the package.
manypkgs::export_data(ECOLEX,
                      database = "agreements",
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