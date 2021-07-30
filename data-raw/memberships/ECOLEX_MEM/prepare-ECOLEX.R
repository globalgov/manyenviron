# ECOLEX_MEM Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.

# Stage one: Collecting data
ECOLEX_MEM <- load("data-raw/memberships/ECOLEX_MEM/ecomembs.RData")
ECOLEX_MEM <- eco_membs
qCreate::retain("ECOLEX_MEM")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'ECOLEX_MEM' object until the object created
# below (in stage three) passes all the tests. 
ECOLEX_MEM <- as_tibble(ECOLEX_MEM) %>%
  dplyr::rename(For = Force,
                Rati = Rat) %>% 
  qData::transmutate(SignatureC = qCreate::standardise_dates(Sign),
                     End = qCreate::standardise_dates(Term),
                     Force = qCreate::standardise_dates(For),
                     Rat = qCreate::standardise_dates(Rati),
                     Country = StatID,
                     ECOLEX_ID = EcolexID) %>%
  dplyr::mutate(Beg = dplyr::coalesce(SignatureC, Rat, Force)) %>% # Check Signature date is for the country and not document signature date
  dplyr::select(ECOLEX_ID, Country, Beg, End, SignatureC, Force, Rat) %>% 
  dplyr::arrange(Beg)

# Add a Title column
ECOLEX <- qEnviron::agreements$ECOLEX %>% 
  dplyr::select(Title, ECOLEX_ID)
ECOLEX_MEM <- merge(ECOLEX_MEM, ECOLEX, by = "ECOLEX_ID", all.x = TRUE)

ECOLEX_MEM$Beg <- qCreate::standardise_dates(ECOLEX_MEM$Beg)
ECOLEX_MEM$End <- qCreate::standardise_dates(ECOLEX_MEM$End)
ECOLEX_MEM$SignatureC <- qCreate::standardise_dates(ECOLEX_MEM$SignatureC)
ECOLEX_MEM$Force <- qCreate::standardise_dates(ECOLEX_MEM$Force)
ECOLEX_MEM$Rat <- qCreate::standardise_dates(ECOLEX_MEM$Rat)

ECOLEX_MEM <- as_tibble(ECOLEX_MEM) %>% 
  dplyr::select(ECOLEX_ID, Country, Title, Beg, End, SignatureC, Rat, Force) %>% 
  dplyr::arrange(Beg)

#Add a qID column
ECOLEX_MEM$qID <- qCreate::code_agreements(ECOLEX_MEM, ECOLEX_MEM$Title, ECOLEX_MEM$Beg)

# qCreate includes several functions that should help cleaning and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make ECOLEX_MEM available within the qPackage.
qCreate::export_data(ECOLEX_MEM, database = "memberships", URL = "https://www.ecolex.org/result/?type=treaty", package = "qEnviron")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence to certain standards.
# You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require you to return
# to stage two and further clean, standardise, or wrangle your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as much detail
# about the variables etc as possible.