# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", memberships[["HUGGO_MEM"]])))
  expect_false(any(grepl("^N/A$", memberships[["HUGGO_MEM"]])))
  expect_false(any(grepl("^\\s$", memberships[["HUGGO_MEM"]])))
  expect_false(any(grepl("^\\.$", memberships[["HUGGO_MEM"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["HUGGO_MEM"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["HUGGO_MEM"]])))
})

# Uniformity tests (agreements have a stateID and Begin columns)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(memberships[["HUGGO_MEM"]],
                                pointblank::vars(stateID))
  pointblank::expect_col_exists(memberships[["HUGGO_MEM"]],
                                pointblank::vars(Begin))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(memberships[["HUGGO_MEM"]])))
  expect_false(any(lubridate::is.POSIXct(memberships[["HUGGO_MEM"]])))
  expect_false(any(lubridate::is.POSIXlt(memberships[["HUGGO_MEM"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Begin` has standardised dates", {
  expect_equal(class(memberships[["HUGGO_MEM"]]$Begin), "mdate")
  expect_false(any(grepl("/", memberships[["HUGGO_MEM"]]$Begin)))
  expect_false(any(grepl("^[:alpha:]$",
                         memberships[["HUGGO_MEM"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         memberships[["HUGGO_MEM"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         memberships[["HUGGO_MEM"]]$Begin)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         memberships[["HUGGO_MEM"]]$Begin)))
})

# Dataset should be ordered according to the "Begin" column
test_that("dataset is arranged by the `Begin` variable", {
  expect_true(memberships[["HUGGO_MEM"]]$Begin[1] <
                memberships[["HUGGO_MEM"]]$Begin[100])
  expect_true(memberships[["HUGGO_MEM"]]$Begin[120] <
                memberships[["HUGGO_MEM"]]$Begin[220])
  expect_true(memberships[["HUGGO_MEM"]]$Begin[250] <
                memberships[["HUGGO_MEM"]]$Begin[350])
})
