# YIO Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for many packages universe.

# Stage one: Creating empty dataset
Title <- 1:75115
YIO <- as.data.frame(Title)

# Stage two: scraping information from https://uia.org/ybio website
# In this stage you will want to correct the variable names and
# formats of the 'YIO' object until the object created
# below (in stage three) passes all the tests.
# Extract first page of website because of different URL

url_1 <- "https://uia.org/ybio" #First page has different URL than other pages
urls <- paste0("https://uia.org/ybio?page=", 1:3004)

extr_title1 <- purrr::map(
  url_1,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-name-en") %>%
    rvest::html_text()
)
extr_title1 <- unlist(extr_title1)
extr_title1 <- extr_title1[-c(1)]


# Extract the rest of the pages of website
extr_titles2 <- tryCatch(purrr::map(
  urls,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-name-en") %>%
    rvest::html_text()
))

#Remove the "Name" observation from top of the page
extr_titles2 <- lapply(extr_titles2, function(x) x[-1])
extr_titles2 <- unlist(extr_titles2)

YIO$Title <- c(extr_title1, extr_titles2)

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

YIO$Abbreviation <- c(extr_abbrev1, extr_abbrev2)

#Clean the columns strings
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

YIO$Beg <- manypkgs::standardise_dates(c(extr_beg1, extr_beg2))

#Clean the column strings
YIO$Beg <- stringr::str_remove_all(YIO$Beg, "\n")
YIO$Beg <- stringr::str_remove_all(YIO$Beg, "\\s\\s")
YIO$Beg <- stringr::str_remove_all(YIO$Beg, "\\s$")
YIO$Beg <- dplyr::na_if(YIO$Beg, "")


# Extract country
extr_country1 <- purrr::map(
  url_1,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-addpays-1-en") %>%
    rvest::html_text()
)
extr_country1 <- unlist(extr_country1)
extr_country1 <- extr_country1[-c(1)]

extr_country2 <- purrr::map(
  urls,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-addpays-1-en") %>%
    rvest::html_text()
)

extr_country2 <- lapply(extr_country2, function(x) x[-1])
extr_country2 <- unlist(extr_country2)

YIO$Country <- c(extr_country1,extr_country2)

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

# extr_city2 <- purrr::map(
#   urls,
#   . %>%
#     rvest::read_html() %>%
#     rvest::html_nodes(".views-field-addcity-1-en") %>%
#     rvest::html_text()
# )
# 
# # Organisation type I
extr_typeI1 <- purrr::map(
  url_1,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-type1") %>%
    rvest::html_text()
)
extr_typeI1 <- unlist(extr_typeI1)
extr_typeI1 <- extr_typeI1[-c(1)]

# extr_typeI2 <- purrr::map(
#   urls,
#   . %>%
#     rvest::read_html() %>%
#     rvest::html_nodes(".views-field-type1") %>%
#     rvest::html_text()
# )
# 

# Organisation type II
extr_typeI2 <- purrr::map(
  url_1,
  . %>%
    rvest::read_html() %>%
    rvest::html_nodes(".views-field-type2") %>%
    rvest::html_text()
)
extr_typeI2 <- unlist(extr_typeI2)
extr_typeI2 <- extr_typeI2[-c(1)]

# extr_typeI2 <- purrr::map(
#   urls,
#   . %>%
#     rvest::read_html() %>%
#     rvest::html_nodes(".views-field-type2") %>%
#     rvest::html_text()
# )
# 

# Organisation type II
# extr_YIO_ID <- purrr::map(
#   url_1,
#   . %>%
#     rvest::read_html() %>%
#     rvest::html_nodes(".views-field-uiaid") %>%
#     rvest::html_text()
# )
# extr_YIO_ID <- unlist(extr_YIO_ID)
# extr_YIO_ID <- extr_YIO_ID[-c(1)]

# extr_YIO_ID2 <- purrr::map(
#   urls,
#   . %>%
#     rvest::read_html() %>%
#     rvest::html_nodes(".footable-last-column") %>%
#     rvest::html_text()
# )
# 



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
