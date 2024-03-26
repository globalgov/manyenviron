# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", references[["ECOLEX_REF"]])))
  expect_false(any(grepl("^N/A$", references[["ECOLEX_REF"]])))
  expect_false(any(grepl("^\\s$", references[["ECOLEX_REF"]])))
  expect_false(any(grepl("^\\.$", references[["ECOLEX_REF"]])))
  expect_false(any(grepl("N\\.A\\.$", references[["ECOLEX_REF"]])))
  expect_false(any(grepl("n\\.a\\.$", references[["ECOLEX_REF"]])))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(references[["ECOLEX_REF"]])))
  expect_false(any(lubridate::is.POSIXct(references[["ECOLEX_REF"]])))
  expect_false(any(lubridate::is.POSIXlt(references[["ECOLEX_REF"]])))
})
