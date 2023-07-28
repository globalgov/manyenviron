# IEADB Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the many package.

# Stage one: Collecting data
IEADB <- readr::read_delim("data-raw/agreements/IEADB/treaties.csv", ",")
# Add Agreement under the United Nations Convention on the Law of the Sea on
# the conservation and sustainable use of marine biological diversity of areas
# beyond national jurisdiction (BBNJ agreement), adopted on June 20, 2023
bbnj <- cbind(9112, "MEA", 2023, "2023-06-23", NA,
              paste("Agreement under the", "United Nations Convention on the",
              "Law of the Sea on the conservation and", "sustainable use of marine",
              "biological diversity of areas beyond national", "jurisdiction",
              sep = " "), "TreatyText**", "Members", "Agreement",
              "Biological Diversity Beyond National Jurisdiction", 100.000, NA,
              NA, "Secretariat not yet identified", NA)
colnames(bbnj) <- colnames(IEADB)
IEADB <- rbind(IEADB, bbnj)


# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'IEADB' object until the object created
# below (in stage three) passes all the tests.

IEADB <- as_tibble(IEADB)  %>%
  dplyr::mutate(AgreementType = dplyr::recode(`Agreement Type (level 2)`,
                                              "Agreement" = "A", "Amendment" = "E",
                                              "Agreed Minute (non-binding)" = "Q",
                                              "Declaration" = "V", "Resolution" = "W",
                                              "Exchange of Notes" = "X",
                                              "Memorandum of Understanding" = "Y",
                                              "Protocol" = "P")) %>%
  dplyr::mutate(DocType = dplyr::recode(Inclusion, "BEA" = "B", "MEA" = "M")) %>%
  dplyr::filter(DocType == "M" | DocType == "B") %>%
  manydata::transmutate(ieadbID = as.character(`IEA# (click for add'l info)`),
                        Title = manypkgs::standardise_titles(`Treaty Name`),
                        Signature = messydates::as_messydate(`Signature Date`),
                        Force = messydates::as_messydate(`Date IEA entered into force`)) %>%
  dplyr::mutate(Begin = dplyr::coalesce(Signature, Force)) %>%
  dplyr::select(ieadbID, Title, Begin, DocType, AgreementType, Signature, Force) %>%
  dplyr::mutate(Begin = messydates::as_messydate(ifelse(is.na(Begin), "9999-12-31", Begin))) %>%
  # Add future date in cases where Begin and force are missing
  dplyr::arrange(Signature)

# Add treatyID column
IEADB$treatyID <- manypkgs::code_agreements(IEADB, IEADB$Title, IEADB$Begin)
# Add Lineage column
IEADB$Lineage <- manypkgs::code_lineage(IEADB$Title)

# Add manyID column
manyID <- manypkgs::condense_agreements(manyenviron::agreements)
IEADB <- dplyr::left_join(IEADB, manyID, by = "treatyID") %>%
   dplyr::distinct() %>%
  dplyr::relocate(manyID, Title, Begin, DocType, AgreementType, Signature,
                Force, Lineage, treatyID, ieadbID) %>%
  dplyr::arrange(Begin)

# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IEADB available
# within the package.
manypkgs::export_data(IEADB, datacube = "agreements",
                      URL = "https://iea.uoregon.edu/base-agreement-list")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure
# adherence to certain standards. You can hit Cmd-Shift-T (Mac)
# or Ctrl-Shift-T (Windows) to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill
# in as much detail about the variables etc as possible.
