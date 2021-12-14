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
  dplyr::mutate(D=dplyr::recode(T, G="A", M="E", "T"="Q", D="V", R="W", N="X", U="Y")) %>% 
  manydata::transmutate(Signature = manypkgs::standardise_dates(DocSign),
                     End = manypkgs::standardise_dates(DocEnd),
                     Force = manypkgs::standardise_dates(DocForce),
                     GNEVAR_ID = GENG,
                     ECOLEX_ID = ECOLEX) %>% 
  dplyr::mutate(Title = manypkgs::standardise_titles(Title, api_key = api)) %>% # Define Key API
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>% 
  dplyr::select(GNEVAR_ID, Title, Beg, End, L,J,D, Signature, Force) %>% 
  dplyr::arrange(Beg)

# Add treaty_ID column
GNEVAR$treaty_ID <- manypkgs::code_agreements(GNEVAR, GNEVAR$Title, GNEVAR$Beg)

# # Clean GNEVAR 2
GNEVAR2 <- as_tibble(GNEVAR2) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title, api_key = api)) %>% # Define Key API
  dplyr::mutate(Beg = manypkgs::standardise_dates(Beg)) %>% 
  dplyr::mutate(End = manypkgs::standardise_dates(End)) %>% 
  dplyr::mutate(Force = manypkgs::standardise_dates(Force)) %>% 
  dplyr::mutate(Term = manypkgs::standardise_dates(Term)) %>% 
  manydata::transmutate(Signature = manypkgs::standardise_dates(Sign))

# Add treaty_ID column
GNEVAR2$treaty_ID <- manypkgs::code_agreements(GNEVAR2, GNEVAR2$Title, GNEVAR2$Beg)

# Clean GNEVAR3 is the same as GNEVAR, no need to include it

# Clean GNEVAR4
GNEVAR4$Parties <- paste0(GNEVAR4$Country.x, "-", GNEVAR4$Country.y)
GNEVAR4 <- as_tibble(GNEVAR4) %>%
  manydata::transmutate(Signature = manypkgs::standardise_dates(DocDate),
                     Force = manypkgs::standardise_dates(InForce)) %>%
  dplyr::mutate(End = manypkgs::standardise_dates(End)) %>%
  dplyr::mutate(Title = manypkgs::standardise_titles(Title, api_key = api)) %>% # Define Key API
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::select(Title, Beg, Signature, Force, End, Parties)

# Add treaty_ID column
GNEVAR4$treaty_ID <- manypkgs::code_agreements(GNEVAR4, GNEVAR4$Title, GNEVAR4$Beg)

# Clean GNEVAR5: the current ID format (MGENG-002) is not found in other GNEVAR datasets
# Can not integrate it into GNEVAR

# Create a GNEVAR "database" to apply consolidate()
GNEVAR <- list(GNEVAR, GNEVAR2, GNEVAR4)

# Join the datasets together
GNEVAR <- manydata::consolidate(GNEVAR, row = "any", cols = "any", resolve = "coalesce", key = "treaty_ID")

# Add many_ID column
many_ID <- manypkgs::condense_agreements(manyenviron::agreements)
GNEVAR <- dplyr::left_join(GNEVAR, many_ID, by = "treaty_ID")

# Select and arrange columns
GNEVAR <- GNEVAR %>% 
  dplyr::select(many_ID, Title, Beg, End, L, D, J, Signature, Force, treaty_ID, GNEVAR_ID) %>% 
  dplyr::arrange(Beg)

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make GNEVAR available
# within the package.
manypkgs::export_data(GNEVAR, database = "agreements", URL = "NA")

# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible