# YIO Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for many packages universe.

# Stage one: Creating empty dataset
Title <- 1:25
YIO1 <- as.data.frame(Title)


# Stage two: scraping information from https://uia.org/ybio website
# In this stage you will want to correct the variable names and
# formats of the 'YIO' object until the object created
# below (in stage three) passes all the tests.
url_1 <- "https://uia.org/ybio"

urls <- paste0("https://uia.org/ybio?page=", 1:3002)

extr_title <- purrr::map(
  url_1,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-name-en") %>%
    rvest::html_text()
)
extr_title <- unlist(extr_title)
extr_title <- extr_title[-c(1)]

YIO1$Title <- extr_title


extr_titles <- tryCatch(purrr::map(
  urls,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-name-en") %>%
    rvest::html_text()
))

extr_titles <- lapply(extr_titles, function(x) x[-1])
extr_titles <- unlist(extr_titles)

Title <- 1:75046
YIO2 <- as.data.frame(Title)

YIO2$Title <- extr_titles

YIO <- as_tibble(rbind(YIO1, YIO2))

YIO$Title <- stringr::str_remove_all(YIO$Title, "\n")
YIO$Title <- stringr::str_remove_all(YIO$Title, "\\s\\s")
YIO$Title <- stringr::str_remove_all(YIO$Title, "\\s$")


# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make YIO available
# within the package.
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
# run `manypkgs::add_bib(organizations, YIO)`.
manypkgs::export_data(YIO, database = "organizations",
                     URL = "https://uia.org/ybio")
