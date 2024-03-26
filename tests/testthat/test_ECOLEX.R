# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", agreements[["ECOLEX"]])))
  expect_false(any(grepl("^N/A$", agreements[["ECOLEX"]])))
  expect_false(any(grepl("^\\s$", agreements[["ECOLEX"]])))
  expect_false(any(grepl("^\\.$", agreements[["ECOLEX"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["ECOLEX"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["ECOLEX"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(agreements[["ECOLEX"]],
                                pointblank::vars(Title))
  pointblank::expect_col_exists(agreements[["ECOLEX"]],
                                pointblank::vars(Begin))
  expect_true(any(grepl("ID$", colnames(agreements[["ECOLEX"]]))))
  pointblank::expect_col_exists(agreements[["ECOLEX"]],
                                pointblank::vars(Signature))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["ECOLEX"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["ECOLEX"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["ECOLEX"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class(agreements[["ECOLEX"]]$Begin), "mdate")
  expect_false(any(grepl("/", agreements[["ECOLEX"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["ECOLEX"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["ECOLEX"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["ECOLEX"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["ECOLEX"]]$Begin)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class(agreements[["ECOLEX"]]$Signature), "mdate")
  expect_false(any(grepl("/", agreements[["ECOLEX"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["ECOLEX"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["ECOLEX"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["ECOLEX"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["ECOLEX"]]$Signature)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["ECOLEX"]]$Begin[1] <
                agreements[["ECOLEX"]]$Begin[10])
  expect_true(agreements[["ECOLEX"]]$Begin[50] <
                agreements[["ECOLEX"]]$Begin[75])
  expect_true(agreements[["ECOLEX"]]$Begin[100] <
                agreements[["ECOLEX"]]$Begin[120])
})
