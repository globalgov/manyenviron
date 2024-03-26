# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", agreements[["IEADB"]])))
  expect_false(any(grepl("^N/A$", agreements[["IEADB"]])))
  expect_false(any(grepl("^\\s$", agreements[["IEADB"]])))
  expect_false(any(grepl("^\\.$", agreements[["IEADB"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["IEADB"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["IEADB"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(agreements[["IEADB"]],
                                pointblank::vars(Title))
  pointblank::expect_col_exists(agreements[["IEADB"]],
                                pointblank::vars(Begin))
  expect_true(any(grepl("ID$", colnames(agreements[["IEADB"]]))))
  pointblank::expect_col_exists(agreements[["IEADB"]],
                                pointblank::vars(Signature))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["IEADB"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["IEADB"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["IEADB"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class(agreements[["IEADB"]]$Begin), "mdate")
  expect_false(any(grepl("/", agreements[["IEADB"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["IEADB"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["IEADB"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["IEADB"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["IEADB"]]$Begin)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class(agreements[["IEADB"]]$Signature), "mdate")
  expect_false(any(grepl("/", agreements[["IEADB"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["IEADB"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["IEADB"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["IEADB"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["IEADB"]]$Signature)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["IEADB"]]$Begin[1] <
                agreements[["IEADB"]]$Begin[10])
  expect_true(agreements[["IEADB"]]$Begin[50] <
                agreements[["IEADB"]]$Begin[75])
  expect_true(agreements[["IEADB"]]$Begin[100] <
                agreements[["IEADB"]]$Begin[120])
})
