# IEADB_TXT Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.

# Stage one: Collecting data
IEADB_TXT <- readr::read_csv("data-raw/texts/IEADB_TXT/IEADB_Text.csv")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'IEADB_TXT' object until the object created
# below (in stage three) passes all the tests.
# First step: rename, extract and standardise useful variables
IEADB_TXT <- as_tibble(IEADB_TXT) %>%
  dplyr::rename(ID = `IEA# (click for add'l info)`,
                TreatyText = `Treaty Text`) %>% 
  qData::transmutate(Title = standardise_titles(`Treaty Name`),
                     Beg = qCreate::standardise_dates(`Signature Date`)) %>% 
  dplyr::select(ID, TreatyText, Title, Beg) %>% 
  dplyr::filter(TreatyText == "Treaty Text**") %>% 
  dplyr::arrange(ID)

# Step two: add qID column
IEADB_TXT$qID <- qCreate::code_agreements(IEADB_TXT, IEADB_TXT$Title, IEADB_TXT$Beg)

# Step three: add a column with treaty texts by web scraping IEADB pages
base <- "https://iea.uoregon.edu/treaty-text/"
IEADB_TXT$TreatyText <- lapply(IEADB_TXT$ID, function(s) tryCatch(rvest::read_html(paste0(base, s)) %>% rvest::html_text(), error = function(e){as.character("Not found")}))

# Step four: keep only the treaty text from the website text
# IEADB_TEXT1
IEADB_TXT$TreatyText_Shorter <- lapply(IEADB_TXT$TreatyText, function(s) gsub(".*Source:","", s))
IEADB_TXT$TreatyText_Shorter <- lapply(IEADB_TXT$TreatyText_Shorter, function(s) gsub("Citation.*","", s))

# Step five: remove variable with all the webpages text and rename text variable
IEADB_TXT = subset(IEADB_TXT, select = - c(TreatyText))
IEADB_TXT <- IEADB_TXT %>% 
  dplyr::rename(Text = TreatyText_Shorter)

# qCreate includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IEADB_TXT available
# within the qPackage.
qCreate::export_data(IEADB_TXT, database = "texts", URL = "https://iea.uoregon.edu/text-index")
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
