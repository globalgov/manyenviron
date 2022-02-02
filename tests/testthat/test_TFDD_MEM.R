# Test if  meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", memberships[["TFDD_MEM"]])))
  expect_false(any(grepl("^N/A$", memberships[["TFDD_MEM"]])))
  expect_false(any(grepl("^\\s$", memberships[["TFDD_MEM"]])))
  expect_false(any(grepl("^\\.$", memberships[["TFDD_MEM"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["TFDD_MEM"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["TFDD_MEM"]])))
})

# Uniformity tests (agreements have a countryID and Beg columns)
test_that("datasets have the required variables", {
  expect_col_exists(memberships[["TFDD_MEM"]], vars(CountryID))
  expect_col_exists(memberships[["TFDD_MEM"]], vars(Beg))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(memberships[["TFDD_MEM"]])))
  expect_false(any(lubridate::is.POSIXct(memberships[["TFDD_MEM"]])))
  expect_false(any(lubridate::is.POSIXlt(memberships[["TFDD_MEM"]])))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class(memberships[["TFDD_MEM"]]$Beg), "messydt")
  expect_false(any(grepl("/", memberships[["TFDD_MEM"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$",
                         memberships[["TFDD_MEM"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         memberships[["TFDD_MEM"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         memberships[["TFDD_MEM"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         memberships[["TFDD_MEM"]]$Beg)))
})

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by the `Beg` variable", {
  expect_true(memberships[["TFDD_MEM"]]$Beg[1] <
                memberships[["TFDD_MEM"]]$Beg[100])
  expect_true(memberships[["TFDD_MEM"]]$Beg[120] <
                memberships[["TFDD_MEM"]]$Beg[220])
  expect_true(memberships[["TFDD_MEM"]]$Beg[250] <
                memberships[["TFDD_MEM"]]$Beg[350])
})
