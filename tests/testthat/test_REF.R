# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", references[["REF"]])))
  expect_false(any(grepl("^N/A$", references[["REF"]])))
  expect_false(any(grepl("^\\s$", references[["REF"]])))
  expect_false(any(grepl("^\\.$", references[["REF"]])))
  expect_false(any(grepl("N\\.A\\.$", references[["REF"]])))
  expect_false(any(grepl("n\\.a\\.$", references[["REF"]])))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(lubridate::is.Date(references[["REF"]]))
  expect_false(lubridate::is.POSIXct(references[["REF"]]))
  expect_false(lubridate::is.POSIXlt(references[["REF"]]))
})
