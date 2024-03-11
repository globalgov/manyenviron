# IRD Preparation Script

# This is a template for importing, cleaning, and exporting data
# for the 'many' packages.

# Stage one: Collecting data
IRD <- readxl::read_excel("data-raw/agreements/IRD/IRD.xlsx")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'IRD' object until the object created
# below (in stage three) passes all the tests.
IRD <- as_tibble(IRD) %>%
  manydata::transmutate(Title = manypkgs::standardise_titles(Regime),
                        Begin = messydates::as_messydate(Formation),
                        End = messydates::as_messydate(as.character(Endpoint)),
                        # Watershed and Watershed2 refer to periods of
                        # fundamental change in the regime
                        Wat = messydates::as_messydate(Watershed),
                        Wat2 = messydates::as_messydate(Watershed2)) %>%
  dplyr::arrange(Begin) %>%
  # RegimeComponent is an institutional arrangement that is
  # part of a given regime, such as a protocol
  # RegimeElement refers to a regulatory area of a given regime for a given time period
  dplyr::select(Title, RegimeComponent, Begin, Wat, Wat2, End, RegimeElement)

# Add treatyID column
IRD$treatyID <- manypkgs::code_agreements(IRD,
                                          IRD$Title,
                                          IRD$Begin)
# Add manyID column
manyID <- manypkgs::condense_agreements(manyenviron::agreements)
IRD <- dplyr::left_join(IRD, manyID, by = "treatyID")

# Re-order the columns
IRD <- IRD %>%
  dplyr::relocate(manyID, Title, RegimeComponent, Begin, Wat, Wat2, End,
                  RegimeElement, treatyID)

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IRD available within the package.
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence
# to certain standards.You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows)
# to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please note that the export_data() function requires a .bib file to be
# present in the data_raw folder of the package for citation purposes.
# Therefore, please make sure that you have permission to use the dataset
# that you're including in the package.
# To add a template of .bib file to package,
# run `manypkgs::add_bib(regimes, IRD)`.
manypkgs::export_data(IRD, datacube = "agreements",
                      URL = "https://direct.mit.edu/glep/article-abstract/6/3/121/14360/The-International-Regimes-Database-Designing-and")
