# Test if  meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", texts[["AGR_TXT"]])))
  expect_false(any(grepl("^N/A$", texts[["AGR_TXT"]])))
  expect_false(any(grepl("^\\s$", texts[["AGR_TXT"]])))
  expect_false(any(grepl("^\\.$", texts[["AGR_TXT"]])))
  expect_false(any(grepl("N\\.A\\.$", texts[["AGR_TXT"]])))
  expect_false(any(grepl("n\\.a\\.$", texts[["AGR_TXT"]])))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(texts[["AGR_TXT"]])))
  expect_false(any(lubridate::is.POSIXct(texts[["AGR_TXT"]])))
  expect_false(any(lubridate::is.POSIXlt(texts[["AGR_TXT"]])))
})
