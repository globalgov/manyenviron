# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", agreements[["HUGGO"]])))
  expect_false(any(grepl("^N/A$", agreements[["HUGGO"]])))
  expect_false(any(grepl("^\\s$", agreements[["HUGGO"]])))
  expect_false(any(grepl("^\\.$", agreements[["HUGGO"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["HUGGO"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["HUGGO"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(agreements[["HUGGO"]],
                                pointblank::vars(Title))
  pointblank::expect_col_exists(agreements[["HUGGO"]],
                                pointblank::vars(Beg))
  expect_true(any(grepl("ID$", colnames(agreements[["HUGGO"]]))))
  pointblank::expect_col_exists(agreements[["HUGGO"]],
                                pointblank::vars(Signature))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["HUGGO"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["HUGGO"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["HUGGO"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class(agreements[["HUGGO"]]$Beg), "mdate")
  expect_false(any(grepl("/", agreements[["HUGGO"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["HUGGO"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["HUGGO"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["HUGGO"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["HUGGO"]]$Beg)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class(agreements[["HUGGO"]]$Signature), "mdate")
  expect_false(any(grepl("/", agreements[["HUGGO"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["HUGGO"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["HUGGO"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["HUGGO"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["HUGGO"]]$Signature)))
})

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["HUGGO"]]$Beg[1] <
                agreements[["HUGGO"]]$Beg[10])
  expect_true(agreements[["HUGGO"]]$Beg[50] <
                agreements[["HUGGO"]]$Beg[75])
  expect_true(agreements[["HUGGO"]]$Beg[100] <
                agreements[["HUGGO"]]$Beg[120])
})
