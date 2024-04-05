# IEA_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# for the 'many' packages.

# Stage one: Collecting data
IEADB_MEM <- readxl::read_excel("data-raw/memberships/IEADB_MEM/iea-memb.xlsx")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'IEA_MEM' object until the object created
# below (in stage three) passes all the tests.
IEADB_MEM <- as_tibble(IEADB_MEM) %>%
  manydata::transmutate(stateID = manypkgs::code_states(country,
                                                        activity = FALSE,
                                                        replace = "ID"),
                        Title = manypkgs::standardise_titles(treatyname),
                       Signature = messydates::as_messydate(tsig),
                       stateSignature = messydates::as_messydate(csig),
                       Rat = messydates::as_messydate(crat),
                       End = messydates::as_messydate(tterm),
                       Force = messydates::as_messydate(ceif3),
                       Force2 = messydates::as_messydate(ceif4),
                       ieadbID = mitch_id) %>%
  dplyr::mutate(DocType = dplyr::recode(inclusion,
                                        "BEA" = "B", "MEA" = "M")) %>%
  dplyr::select(stateID, Title, Signature, End, Rat, Force,
                Force2, stateSignature, DocType, ieadbID) %>%
  tidyr::pivot_longer(c(Force2, Force), values_to = "Force") %>%
  dplyr::filter(!is.na(Force) & name != "Force2") %>%
  dplyr::mutate(Begin = dplyr::coalesce(stateSignature, Rat, Force)) %>%
  dplyr::select(stateID, Title, Begin, End, stateSignature,
                Signature, Rat, Force, DocType, ieadbID) %>%
  dplyr::arrange(Begin)

# Add a treatyID column
IEADB_MEM$treatyID <- manypkgs::code_agreements(IEADB_MEM,
                                                IEADB_MEM$Title,
                                                IEADB_MEM$Begin)

# Add manyID column
manyID <- manypkgs::condense_agreements(manyenviron::memberships)
IEADB_MEM <- dplyr::left_join(IEADB_MEM, manyID, by = "treatyID") %>% 
  dplyr::distinct() %>%
  dplyr::relocate(manyID)

# manypkgs includes several functions that should help
# cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IEA_MEM available
# within the package.
manypkgs::export_data(IEADB_MEM,
                      datacube = "memberships",
                      URL = "https://iea.uoregon.edu/country-members")
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
