# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^N/A$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^\\s$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^\\.$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["ECOLEX_MEM"]])))
})

# Uniformity tests (agreements have a countryID and Beg columns)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(memberships[["ECOLEX_MEM"]],
                                pointblank::vars(stateID))
  pointblank::expect_col_exists(memberships[["ECOLEX_MEM"]],
                                pointblank::vars(Beg))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(memberships[["ECOLEX_MEM"]])))
  expect_false(any(lubridate::is.POSIXct(memberships[["ECOLEX_MEM"]])))
  expect_false(any(lubridate::is.POSIXlt(memberships[["ECOLEX_MEM"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class(memberships[["ECOLEX_MEM"]]$Beg), "mdate")
  expect_false(any(grepl("/", memberships[["ECOLEX_MEM"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$",
                         memberships[["ECOLEX_MEM"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         memberships[["ECOLEX_MEM"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         memberships[["ECOLEX_MEM"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         memberships[["ECOLEX_MEM"]]$Beg)))
})

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by the `Beg` variable", {
  expect_true(memberships[["ECOLEX_MEM"]]$Beg[1] <
                memberships[["ECOLEX_MEM"]]$Beg[100])
  expect_true(memberships[["ECOLEX_MEM"]]$Beg[120] <
                memberships[["ECOLEX_MEM"]]$Beg[220])
  expect_true(memberships[["ECOLEX_MEM"]]$Beg[250] <
                memberships[["ECOLEX_MEM"]]$Beg[350])
})
