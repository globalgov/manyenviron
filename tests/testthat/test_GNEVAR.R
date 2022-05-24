# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("^N/A$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("^\\s$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("^\\.$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["GNEVAR"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(agreements[["GNEVAR"]], pointblank::vars(Title))
  pointblank::expect_col_exists(agreements[["GNEVAR"]], pointblank::vars(Beg))
  expect_true(any(grepl("ID$", colnames(agreements[["GNEVAR"]]))))
  pointblank::expect_col_exists(agreements[["GNEVAR"]],
                                pointblank::vars(Signature))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["GNEVAR"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["GNEVAR"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["GNEVAR"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class(agreements[["GNEVAR"]]$Beg), "mdate")
  expect_false(any(grepl("/", agreements[["GNEVAR"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["GNEVAR"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["GNEVAR"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["GNEVAR"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["GNEVAR"]]$Beg)))
})

test_that("Column `Signature` has standardised dates", {
  expect_equal(class(agreements[["GNEVAR"]]$Signature), "mdate")
  expect_false(any(grepl("/", agreements[["GNEVAR"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["GNEVAR"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["GNEVAR"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["GNEVAR"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["GNEVAR"]]$Signature)))
})

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["GNEVAR"]]$Beg[1] <
                agreements[["GNEVAR"]]$Beg[10])
  expect_true(agreements[["GNEVAR"]]$Beg[50] <
                agreements[["GNEVAR"]]$Beg[75])
  expect_true(agreements[["GNEVAR"]]$Beg[100] <
                agreements[["GNEVAR"]]$Beg[120])
})
