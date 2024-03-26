# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", agreements[["HEIDI"]])))
  expect_false(any(grepl("^N/A$", agreements[["HEIDI"]])))
  expect_false(any(grepl("^\\s$", agreements[["HEIDI"]])))
  expect_false(any(grepl("^\\.$", agreements[["HEIDI"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["HEIDI"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["HEIDI"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(agreements[["HEIDI"]],
                                pointblank::vars(Title))
  pointblank::expect_col_exists(agreements[["HEIDI"]],
                                pointblank::vars(Begin))
  expect_true(any(grepl("ID$", colnames(agreements[["HEIDI"]]))))
  pointblank::expect_col_exists(agreements[["HEIDI"]],
                                pointblank::vars(Signature))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["HEIDI"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["HEIDI"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["HEIDI"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class(agreements[["HEIDI"]]$Begin), "mdate")
  expect_false(any(grepl("/", agreements[["HEIDI"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["HEIDI"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["HEIDI"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["HEIDI"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["HEIDI"]]$Begin)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class(agreements[["HEIDI"]]$Signature), "mdate")
  expect_false(any(grepl("/", agreements[["HEIDI"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["HEIDI"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["HEIDI"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["HEIDI"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["HEIDI"]]$Signature)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["HEIDI"]]$Begin[1] <
                agreements[["HEIDI"]]$Begin[10])
  expect_true(agreements[["HEIDI"]]$Begin[50] <
                agreements[["HEIDI"]]$Begin[75])
  expect_true(agreements[["HEIDI"]]$Begin[100] <
                agreements[["HEIDI"]]$Begin[120])
})
