# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", texts[["IEADB_TXT"]])))
  expect_false(any(grepl("^N/A$", texts[["IEADB_TXT"]])))
  expect_false(any(grepl("^\\s$", texts[["IEADB_TXT"]])))
  expect_false(any(grepl("^\\.$", texts[["IEADB_TXT"]])))
  expect_false(any(grepl("N\\.A\\.$", texts[["IEADB_TXT"]])))
  expect_false(any(grepl("n\\.a\\.$", texts[["IEADB_TXT"]])))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(lubridate::is.Date(texts[["IEADB_TXT"]]))
  expect_false(lubridate::is.POSIXct(texts[["IEADB_TXT"]]))
  expect_false(lubridate::is.POSIXlt(texts[["IEADB_TXT"]]))
})
