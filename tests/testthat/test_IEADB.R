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
  pointblank::expect_col_exists(agreements[["IEADB"]], pointblank::vars(Title))
  pointblank::expect_col_exists(agreements[["IEADB"]], pointblank::vars(Beg))
  expect_true(any(grepl("ID$", colnames(agreements[["IEADB"]]))))
  pointblank::expect_col_exists(agreements[["IEADB"]], pointblank::vars(Signature))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["IEADB"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["IEADB"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["IEADB"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class(agreements[["IEADB"]]$Beg), "messydt")
  expect_false(any(grepl("/", agreements[["IEADB"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["IEADB"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["IEADB"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["IEADB"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["IEADB"]]$Beg)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class(agreements[["IEADB"]]$Signature), "messydt")
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

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["IEADB"]]$Beg[1] <
                agreements[["IEADB"]]$Beg[10])
  expect_true(agreements[["IEADB"]]$Beg[50] <
                agreements[["IEADB"]]$Beg[75])
  expect_true(agreements[["IEADB"]]$Beg[100] <
                agreements[["IEADB"]]$Beg[120])
})
