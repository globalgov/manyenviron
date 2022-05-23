# GNEVAR_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many packages universe.

# Stage one: Collecting data
GNEVAR_MEM <- readr::read_csv("data-raw/memberships/GNEVAR_MEM/gnevar.csv")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'GNEVAR_MEM' object until the object created
# below (in stage three) passes all the tests.
GNEVAR_MEM <- as_tibble(GNEVAR_MEM) %>%
  manydata::transmutate(gnevarID = GENG,
                     Rat = manypkgs::messydates::make_messydates(Approval),
                     Withdrawal = manypkgs::messydates::make_messydates(Withdrawal1),
                     Signature = manypkgs::messydates::make_messydates(DocSign),
                     Force = manypkgs::messydates::make_messydates(DocForce),
                     Term = manypkgs::messydates::make_messydates(DocEnd),
                     Force = manypkgs::messydates::make_messydates(InForce1)) %>%
  dplyr::mutate(SignatureCountry = Signature) %>%
  dplyr::mutate(CountryID = Country) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title,
                                                     api_key = api)) %>%
  # Define Key API
  dplyr::mutate(Beg = dplyr::coalesce(SignatureCountry, Rat, Force)) %>%
  dplyr::mutate(End = dplyr::coalesce(Withdrawal, Term)) %>%
  dplyr::select(CountryID, Title, Beg, End, SignatureCountry, Signature,
                Rat, Force, Term, Withdrawal, gnevarID) %>%
  dplyr::arrange(Beg)

# Add treatyID column
GNEVAR_MEM$treatyID <- manypkgs::code_agreements(GNEVAR_MEM,
                                                 GNEVAR_MEM$Title,
                                                 GNEVAR_MEM$Beg)

# Add manyID column
manyID <- manypkgs::condense_agreements(manyenviron::memberships)
GNEVAR_MEM <- dplyr::left_join(GNEVAR_MEM, manyID, by = "treatyID")

# Re-order the columns
GNEVAR_MEM <- dplyr::relocate(GNEVAR_MEM, manyID)

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make GNEVAR_MEM available
# within the package.
manypkgs::export_data(GNEVAR_MEM, database = "memberships", URL = "NA")
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
