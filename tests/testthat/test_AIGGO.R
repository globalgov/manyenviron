# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", agreements[["AIGGO"]])))
  expect_false(any(grepl("^N/A$", agreements[["AIGGO"]])))
  expect_false(any(grepl("^\\s$", agreements[["AIGGO"]])))
  expect_false(any(grepl("^\\.$", agreements[["AIGGO"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["AIGGO"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["AIGGO"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(agreements[["AIGGO"]],
                                pointblank::vars(Title))
  pointblank::expect_col_exists(agreements[["AIGGO"]],
                                pointblank::vars(Begin))
  expect_true(any(grepl("ID$", colnames(agreements[["AIGGO"]]))))
  pointblank::expect_col_exists(agreements[["AIGGO"]],
                                pointblank::vars(Signature))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["AIGGO"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["AIGGO"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["AIGGO"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class(agreements[["AIGGO"]]$Begin), "mdate")
  expect_false(any(grepl("/", agreements[["AIGGO"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["AIGGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["AIGGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["AIGGO"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["AIGGO"]]$Begin)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class(agreements[["AIGGO"]]$Signature), "mdate")
  expect_false(any(grepl("/", agreements[["AIGGO"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["AIGGO"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["AIGGO"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["AIGGO"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["AIGGO"]]$Signature)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["AIGGO"]]$Begin[1] <
                agreements[["AIGGO"]]$Begin[10])
  expect_true(agreements[["AIGGO"]]$Begin[50] <
                agreements[["AIGGO"]]$Begin[75])
  expect_true(agreements[["AIGGO"]]$Begin[100] <
                agreements[["AIGGO"]]$Begin[120])
})
