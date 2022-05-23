# GNEVAR Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many packages universe.

# Stage one: Collecting data
GNEVAR <- readr::read_csv("data-raw/agreements/GNEVAR/EnvGov Nodes-Table 1 VERS2.csv")
GNEVAR2 <- readr::read_csv2("data-raw/agreements/GNEVAR/endb4beg.csv")
GNEVAR3 <- readr::read_csv("data-raw/agreements/GNEVAR/gnevar.csv")
GNEVAR4 <- readr::read_csv("data-raw/agreements/GNEVAR/GENG v1.2 (31.10.2015).csv")
GNEVAR5 <- readr::read_csv2("data-raw/agreements/GNEVAR/duplicates v1.0.csv")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'GNEVAR' object until the object created
# below (in stage three) passes all the tests.
GNEVAR <- as_tibble(GNEVAR)  %>%
  tidyr::separate(IEA, c("NEW", "IEADB_ID"), sep = "-") %>%
  dplyr::mutate(AgreementType=dplyr::recode(T, G="A", M="E", "T"="Q",
                                            D="V", R="W", N="X", U="Y")) %>%
  manydata::transmutate(Signature = messydates::make_messydates(DocSign),
                     End = messydates::make_messydates(DocEnd),
                     Force = messydates::make_messydates(DocForce),
                     gnevarID = GENG,
                     ecolexID = ECOLEX) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title,
                                                     api_key = api)) %>%
  # Define Key API
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::rename(DocType = L) %>%
  dplyr::rename(GeogArea = J) %>%
  dplyr::select(gnevarID, Title, Beg, End, DocType, GeogArea, AgreementType,
                Signature, Force) %>%
  dplyr::arrange(Beg)

# Add treatyID column
GNEVAR$treatyID <- manypkgs::code_agreements(GNEVAR,
                                             GNEVAR$Title,
                                             GNEVAR$Beg)

# # Clean GNEVAR 2
GNEVAR2 <- as_tibble(GNEVAR2) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title,
                                                     api_key = api)) %>%
  # Define Key API
  dplyr::mutate(Beg = messydates::make_messydates(Beg)) %>%
  dplyr::mutate(End = messydates::make_messydates(End)) %>%
  dplyr::mutate(Force = messydates::make_messydates(Force)) %>%
  dplyr::mutate(Term = messydates::make_messydates(Term)) %>%
  manydata::transmutate(Signature = messydates::make_messydates(Sign))

# Add treatyID column
GNEVAR2$treatyID <- manypkgs::code_agreements(GNEVAR2,
                                              GNEVAR2$Title,
                                              GNEVAR2$Beg)

# Clean GNEVAR3 is the same as GNEVAR, no need to include it

# Clean GNEVAR4
GNEVAR4$Parties <- paste0(GNEVAR4$Country.x, "-", GNEVAR4$Country.y)
GNEVAR4 <- as_tibble(GNEVAR4) %>%
  manydata::transmutate(Signature = messydates::make_messydates(DocDate),
                     Force = messydates::make_messydates(InForce)) %>%
  dplyr::mutate(End = messydates::make_messydates(End)) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title,
                                                     api_key = api)) %>%
  # Define Key API
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::select(Title, Beg, Signature, Force, End, Parties)

# Add treatyID column
GNEVAR4$treatyID <- manypkgs::code_agreements(GNEVAR4,
                                              GNEVAR4$Title,
                                              GNEVAR4$Beg)

# Clean GNEVAR5: the current ID format (MGENG-002)
# is not found in other GNEVAR datasets
# Can not integrate it into GNEVAR

# Create a GNEVAR "database" to apply consolidate()
GNEVAR <- list(GNEVAR, GNEVAR2, GNEVAR4)

# Join the datasets together
GNEVAR <- manydata::consolidate(GNEVAR, row = "any", cols = "any",
                                resolve = "coalesce", key = "treatyID")

# Add Lineage Column
GNEVAR$Lineage <- manypkgs::code_lineage(GNEVAR$Title)

# Add membership conditions and procedures columns
AGR_TXT <- manyenviron::texts$AGR_TXT
AGR_TXT$Memb.conditions <- manypkgs::code_memberships(AGR_TXT$Text, 
                                                      AGR_TXT$Title, 
                                                      memberships = "condition")
AGR_TXT$Memb.procedures <- manypkgs::code_memberships(AGR_TXT$Text, 
                                                      memberships = "process")
AGR_TXT <- dplyr::select(AGR_TXT, manyID, Memb.conditions, Memb.procedures)
GNEVAR <- dplyr::left_join(GNEVAR, AGR_TXT,
                           by = "manyID")

# Add manyID column
manyID <- manypkgs::condense_agreements(manyenviron::agreements)
GNEVAR <- dplyr::left_join(GNEVAR, manyID, by = "treatyID")

# Select and arrange columns
GNEVAR <- GNEVAR %>%
  dplyr::select(manyID, Title, Beg, End, DocType, AgreementType, GeogArea,
                Signature, Force, Lineage, Memb.conditions, Memb.procedures, 
                treatyID, gnevarID) %>%
  dplyr::arrange(Beg)

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make GNEVAR available
# within the package.
manypkgs::export_data(GNEVAR, database = "agreements", URL = "NA")
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
