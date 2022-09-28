# HUGGO_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many packages universe.

# Stage one: Collecting data
HUGGO_MEM <- readr::read_csv("data-raw/memberships/HUGGO_MEM/gnevar.csv")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'HUGGO_MEM' object until the object created
# below (in stage three) passes all the tests.
HUGGO_MEM <- as_tibble(HUGGO_MEM) %>%
  manydata::transmutate(HUGGOID = GENG,
                     Rat = messydates::as_messydate(Approval),
                     Withdrawal = messydates::as_messydate(Withdrawal1),
                     Signature = messydates::as_messydate(DocSign),
                     Force = messydates::as_messydate(DocForce),
                     Term = messydates::as_messydate(DocEnd),
                     Force = messydates::as_messydate(InForce1)) %>%
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
HUGGO_MEM$treatyID <- manypkgs::code_agreements(HUGGO_MEM,
                                                HUGGO_MEM$Title,
                                                HUGGO_MEM$Beg)

# Add manyID column
manyID <- manypkgs::condense_agreements(manyenviron::memberships)
HUGGO_MEM <- dplyr::left_join(HUGGO_MEM, manyID, by = "treatyID")

# Re-order the columns
HUGGO_MEM <- dplyr::relocate(HUGGO_MEM, manyID)

# Add MEA edges data (MGENG dataset)
# For some more information about the variables and codes,
# please see the documentation in the data-raw folder.

MEA_edges <- readr::read_delim("data-raw/agreements/HUGGO/MEA.Edges v1.0.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
MEA_edges <- as_tibble(MEA_edges) %>%
  dplyr::select(-'...1') %>%
  dplyr::rename(gengID = GENG.ID,
                memb_sign1 = MembSign.x,
                memb_rat1 = MembRat.x,
                memb_force_ecolex = MembForce.x,
                memb_force_iea = MembForce.y,
                memb_sign2 = MembSign.x2,
                memb_rat2 = MembRat.x2,
                memb_sign3 = MembSign.x3,
                memb_rat3 = MembRat.x3)
names(a) <- gsub("\\.", "", names(a))

# Join data


# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make HUGGO_MEM available
# within the package.
manypkgs::export_data(HUGGO_MEM, database = "memberships", URL = "NA")
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
