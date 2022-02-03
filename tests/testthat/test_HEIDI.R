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
  expect_col_exists(agreements[["HEIDI"]], vars(Title))
  expect_col_exists(agreements[["HEIDI"]], vars(Beg))
  expect_true(any(grepl("ID$", colnames(agreements[["HEIDI"]]))))
  expect_col_exists(agreements[["HEIDI"]], vars(Signature))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["HEIDI"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["HEIDI"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["HEIDI"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class(agreements[["HEIDI"]]$Beg), "messydt")
  expect_false(any(grepl("/", agreements[["HEIDI"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["HEIDI"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["HEIDI"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["HEIDI"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["HEIDI"]]$Beg)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class(agreements[["HEIDI"]]$Signature), "messydt")
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

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["HEIDI"]]$Beg[1] <
                agreements[["HEIDI"]]$Beg[10])
  expect_true(agreements[["HEIDI"]]$Beg[50] <
                agreements[["HEIDI"]]$Beg[75])
  expect_true(agreements[["HEIDI"]]$Beg[100] <
                agreements[["HEIDI"]]$Beg[120])
})
