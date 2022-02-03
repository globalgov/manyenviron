# IEA_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many packages universe.

# Stage one: Collecting data
IEADB_MEM <- readxl::read_excel("data-raw/memberships/IEADB_MEM/iea-memb.xlsx")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'IEA_MEM' object until the object created
# below (in stage three) passes all the tests.
IEADB_MEM <- as_tibble(IEADB_MEM) %>%
  manydata::transmutate(CountryID = qStates::code_states(country),
                     Title = manypkgs::standardise_titles(treatyname,
                                                          api_key = api),
                     # Define Key API
                     Signature = manypkgs::standardise_dates(tsig),
                     SignatureC = manypkgs::standardise_dates(csig),
                     Rat = manypkgs::standardise_dates(crat),
                     End = manypkgs::standardise_dates(tterm),
                     Force = manypkgs::standardise_dates(ceif3),
                     Force2 = manypkgs::standardise_dates(ceif4),
                     ieadbID = mitch_id) %>%
  dplyr::mutate(L = dplyr::recode(inclusion, "BEA" = "B", "MEA" = "M")) %>%
  dplyr::select(CountryID, Title, Signature, End, Rat, Force,
                Force2, SignatureC, L, ieadbID) %>%
  tidyr::pivot_longer(c(Force2, Force), values_to = "Force") %>%
  dplyr::filter(!is.na(Force) & name != "Force2") %>%
  dplyr::mutate(Beg = dplyr::coalesce(SignatureC, Rat, Force)) %>%
  dplyr::select(CountryID, Title, Beg, End, SignatureC,
                Signature, Rat, Force, L, ieadbID) %>%
  dplyr::arrange(Beg)

# Add a treatyID column
IEADB_MEM$treatyID <- manypkgs::code_agreements(IEADB_MEM,
                                                IEADB_MEM$Title,
                                                IEADB_MEM$Beg)

# Add manyID column
manyID <- manypkgs::condense_agreements(manyenviron::memberships)
IEADB_MEM <- dplyr::left_join(IEADB_MEM, manyID, by = "treatyID")

# Re-order the columns
IEADB_MEM <- dplyr::relocate(IEADB_MEM, manyID)

# manypkgs includes several functions that should help 
# cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IEA_MEM available
# within the package.
manypkgs::export_data(IEADB_MEM,
                      database = "memberships",
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