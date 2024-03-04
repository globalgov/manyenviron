# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", agreements[["IRD"]])))
  expect_false(any(grepl("^N/A$", agreements[["IRD"]])))
  expect_false(any(grepl("^\\s$", agreements[["IRD"]])))
  expect_false(any(grepl("^\\.$", agreements[["IRD"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["IRD"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["IRD"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(agreements[["IRD"]],
                                pointblank::vars(Title))
  pointblank::expect_col_exists(agreements[["IRD"]],
                                pointblank::vars(Begin))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["IRD"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["IRD"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["IRD"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class(agreements[["IRD"]]$Begin), "mdate")
  expect_false(any(grepl("/", agreements[["IRD"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["IRD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["IRD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["IRD"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["IRD"]]$Begin)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["IRD"]]$Begin[1] <
                agreements[["IRD"]]$Begin[10])
  expect_true(agreements[["IRD"]]$Begin[20] <
                agreements[["IRD"]]$Begin[30])
  expect_true(agreements[["IRD"]]$Begin[40] <
                agreements[["IRD"]]$Begin[50])
})
