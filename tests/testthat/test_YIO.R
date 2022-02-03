# Test if  meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", organizations[["YIO"]])))
  expect_false(any(grepl("^N/A$", organizations[["YIO"]])))
  expect_false(any(grepl("^\\s$", organizations[["YIO"]])))
  expect_false(any(grepl("^\\.$", organizations[["YIO"]])))
  expect_false(any(grepl("N\\.A\\.$", organizations[["YIO"]])))
  expect_false(any(grepl("n\\.a\\.$", organizations[["YIO"]])))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(organizations[["YIO"]])))
  expect_false(any(lubridate::is.POSIXct(organizations[["YIO"]])))
  expect_false(any(lubridate::is.POSIXlt(organizations[["YIO"]])))
})
