# GNEVAR Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.

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
  qData::transmutate(Signature = qCreate::standardise_dates(DocSign),
                     End = qCreate::standardise_dates(DocEnd),
                     Force = qCreate::standardise_dates(DocForce),
                     GNEVAR_ID = GENG,
                     ECOLEX_ID = ECOLEX) %>% 
  dplyr::mutate(Title = qCreate::standardise_titles(Title)) %>% # Key API has been used to translate 
  # treaties not in english
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>% 
  dplyr::select(GNEVAR_ID, Title, Beg, End, L,J,D, Signature, Force) %>% 
  dplyr::arrange(Beg)

# Add qID column
GNEVAR$qID <- qCreate::code_agreements(GNEVAR, GNEVAR$Title, GNEVAR$Beg)

# # Clean GNEVAR 2
GNEVAR2 <- as_tibble(GNEVAR2) %>%
  dplyr::mutate(Title = qCreate::standardise_titles(Title)) %>% # key API also used here
  dplyr::mutate(Beg = qCreate::standardise_dates(Beg)) %>% 
  dplyr::mutate(End = qCreate::standardise_dates(End)) %>% 
  dplyr::mutate(Force = qCreate::standardise_dates(Force)) %>% 
  dplyr::mutate(Term = qCreate::standardise_dates(Term)) %>% 
  qData::transmutate(Signature = qCreate::standardise_dates(Sign))

# Add qID column
GNEVAR2$qID <- qCreate::code_agreements(GNEVAR2, GNEVAR2$Title, GNEVAR2$Beg)

# Clean GNEVAR3 is the same as GNEVAR, no need to include it

# Clean GNEVAR4
GNEVAR4$Parties <- paste0(GNEVAR4$Country.x, "-", GNEVAR4$Country.y)
GNEVAR4 <- as_tibble(GNEVAR4) %>%
  qData::transmutate(Signature = qCreate::standardise_dates(DocDate),
                     Force = qCreate::standardise_dates(InForce)) %>%
  dplyr::mutate(End = qCreate::standardise_dates(End)) %>%
  dplyr::mutate(Title = qCreate::standardise_titles(Title)) %>% # Key API used here
  dplyr::mutate(Beg = dplyr::coalesce(Signature, Force)) %>%
  dplyr::select(Title, Beg, Signature, Force, End, Parties)

# Add qID column
GNEVAR4$qID <- qCreate::code_agreements(GNEVAR4, GNEVAR4$Title, GNEVAR4$Beg)

# Clean GNEVAR5: the current ID format (MGENG-002) is not found in other GNEVAR datasets
# Can not integrate it into GNEVAR

# Create a GNEVAR "database" to apply consolidate()
GNEVAR <- list(GNEVAR, GNEVAR2, GNEVAR4)

# Join the datasets together
GNEVAR <- qData::consolidate(GNEVAR, row = "any", cols = "any", key = "qID")

# Add qID_ref column
qID_ref <- qCreate::condense_qID(qEnviron::agreements)
GNEVAR <- dplyr::left_join(GNEVAR, qID_ref, by = "qID")

# Select and arrange columns
GNEVAR <- GNEVAR %>% 
  dplyr::select(qID_ref, Title, Beg, End, L, D, J, Signature, Force, qID, GNEVAR_ID) %>% 
  dplyr::arrange(Beg)

# qCreate includes several functions that should help cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make GNEVAR available within the qPackage.
qCreate::export_data(GNEVAR, database = "agreements", URL = "NA")

# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible