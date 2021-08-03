# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", references[["ECOLEX_REF"]])))
  expect_false(any(grepl("^N/A$", references[["ECOLEX_REF"]])))
  expect_false(any(grepl("^\\s$", references[["ECOLEX_REF"]])))
  expect_false(any(grepl("^\\.$", references[["ECOLEX_REF"]])))
  expect_false(any(grepl("N\\.A\\.$", references[["ECOLEX_REF"]])))
  expect_false(any(grepl("n\\.a\\.$", references[["ECOLEX_REF"]])))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(lubridate::is.Date(references[["ECOLEX_REF"]]))
  expect_false(lubridate::is.POSIXct(references[["ECOLEX_REF"]]))
  expect_false(lubridate::is.POSIXlt(references[["ECOLEX_REF"]]))
})
