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
# Extract first page of website because of different URL
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

# Extract the rest of the pages of website
extr_titles <- tryCatch(purrr::map(
  urls,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-name-en") %>%
    rvest::html_text()
))

#Remove the "Name" observation from top of the page
extr_titles <- lapply(extr_titles, function(x) x[-1])
extr_titles <- unlist(extr_titles)

Title <- 1:75046
YIO2 <- as.data.frame(Title)

YIO2$Title <- extr_titles

YIO <- as_tibble(rbind(YIO1, YIO2))

# Clean the output
YIO$Title <- stringr::str_remove_all(YIO$Title, "\n")
YIO$Title <- stringr::str_remove_all(YIO$Title, "\\s\\s")
YIO$Title <- stringr::str_remove_all(YIO$Title, "\\s$")


# Extract abbreviationss
extr_abbrev1 <- purrr::map(
  url_1,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-abbr-en") %>%
    rvest::html_text()
)
extr_abbrev1 <- unlist(extr_abbrev1)
extr_abbrev1 <- extr_abbrev1[-c(1)]

extr_abbrev2 <- tryCatch(purrr::map(
  urls,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-abbr-en") %>%
    rvest::html_text()
))

#Remove the "Name" observation from top of the page
extr_abbrev2 <- lapply(extr_abbrev2, function(x) x[-1])
extr_abbrev2 <- unlist(extr_abbrev2)

extr_abbrev <- c(extr_abbrev1, extr_abbrev2)

YIO$Abbreviation <- extr_abbrev

YIO$Abbreviation <- stringr::str_remove_all(YIO$Abbreviation, "\n")
YIO$Abbreviation <- stringr::str_remove_all(YIO$Abbreviation, "\\s\\s")
YIO$Abbreviation <- stringr::str_remove_all(YIO$Abbreviation, "\\s$")
YIO$Abbreviation <- dplyr::na_if(YIO$Abbreviation, "")

# Extract year of creation
extr_beg1 <- purrr::map(
  url_1,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-birthyear") %>%
    rvest::html_text()
)
extr_beg1 <- unlist(extr_beg1)
extr_beg1 <- extr_beg1[-c(1)]

extr_beg2 <- purrr::map(
  urls,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-birthyear") %>%
    rvest::html_text()
)
extr_beg2 <- lapply(extr_beg2, function(x) x[-1])
extr_beg2 <- unlist(extr_beg2)

YIO$Beg <- c(extr_beg1, extr_beg2)

YIO$Beg <- stringr::str_remove_all(YIO$Beg, "\n")
YIO$Beg <- stringr::str_remove_all(YIO$Beg, "\\s\\s")
YIO$Beg <- stringr::str_remove_all(YIO$Beg, "\\s$")
YIO$Beg <- dplyr::na_if(YIO$Beg, "")

YIO$Beg <- manypkgs::standardise_dates(YIO$Beg)

# Extract city of HQ
extr_city1 <- purrr::map(
  url_1,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-addcity-1-en") %>%
    rvest::html_text()
)
extr_city1 <- unlist(extr_city1)
extr_city1 <- extr_city1[-c(1)]

extr_city2 <- purrr::map(
  urls,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-addcity-1-en") %>%
    rvest::html_text()
)


# # Extract country
# extr_country1 <- purrr::map(
#   url_1,
#   . %>%
#     rvest::read_html() %>%
#     rvest::html_nodes(".views-field-addpays-1-en") %>%
#     rvest::html_text()
# )
# extr_country1 <- unlist(extr_country1)
# extr_country1 <- extr_country1[-c(1)]
# 
# extr_country2 <- purrr::map(
#   urls,
#   . %>%
#     rvest::read_html() %>%
#     rvest::html_nodes(".views-field-addpays-1-en") %>%
#     rvest::html_text()
# )


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
