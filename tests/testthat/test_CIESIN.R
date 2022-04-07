# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", agreements[["CIESIN"]])))
  expect_false(any(grepl("^N/A$", agreements[["CIESIN"]])))
  expect_false(any(grepl("^\\s$", agreements[["CIESIN"]])))
  expect_false(any(grepl("^\\.$", agreements[["CIESIN"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["CIESIN"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["CIESIN"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(agreements[["CIESIN"]], vars(Title))
  pointblank::expect_col_exists(agreements[["CIESIN"]], vars(Beg))
  expect_true(any(grepl("ID$", colnames(agreements[["CIESIN"]]))))
  pointblank::expect_col_exists(agreements[["CIESIN"]], vars(Signature))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["CIESIN"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["CIESIN"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["CIESIN"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class(agreements[["CIESIN"]]$Beg), "messydt")
  expect_false(any(grepl("/", agreements[["CIESIN"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["CIESIN"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["CIESIN"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["CIESIN"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["CIESIN"]]$Beg)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class(agreements[["CIESIN"]]$Signature), "messydt")
  expect_false(any(grepl("/", agreements[["CIESIN"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["CIESIN"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["CIESIN"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["CIESIN"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["CIESIN"]]$Signature)))
})

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["CIESIN"]]$Beg[1] <
                agreements[["CIESIN"]]$Beg[10])
  expect_true(agreements[["CIESIN"]]$Beg[50] <
                agreements[["CIESIN"]]$Beg[75])
  expect_true(agreements[["CIESIN"]]$Beg[100] <
                agreements[["CIESIN"]]$Beg[120])
})
