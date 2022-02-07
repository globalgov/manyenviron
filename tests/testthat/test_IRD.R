# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", regimes[["IRD"]])))
  expect_false(any(grepl("^N/A$", regimes[["IRD"]])))
  expect_false(any(grepl("^\\s$", regimes[["IRD"]])))
  expect_false(any(grepl("^\\.$", regimes[["IRD"]])))
  expect_false(any(grepl("N\\.A\\.$", regimes[["IRD"]])))
  expect_false(any(grepl("n\\.a\\.$", regimes[["IRD"]])))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(regimes[["IRD"]])))
  expect_false(any(lubridate::is.POSIXct(regimes[["IRD"]])))
  expect_false(any(lubridate::is.POSIXlt(regimes[["IRD"]])))
})
