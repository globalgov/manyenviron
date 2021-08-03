# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", memberships[["TFDD_MEM"]])))
  expect_false(any(grepl("^N/A$", memberships[["TFDD_MEM"]])))
  expect_false(any(grepl("^\\s$", memberships[["TFDD_MEM"]])))
  expect_false(any(grepl("^\\.$", memberships[["TFDD_MEM"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["TFDD_MEM"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["TFDD_MEM"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  expect_col_exists(memberships[["TFDD_MEM"]], vars(Country))
  expect_col_exists(memberships[["TFDD_MEM"]], vars(Beg))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(lubridate::is.Date(memberships[["TFDD_MEM"]]))
  expect_false(lubridate::is.POSIXct(memberships[["TFDD_MEM"]]))
  expect_false(lubridate::is.POSIXlt(memberships[["TFDD_MEM"]]))
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
                memberships[["TFDD_MEM"]]$Beg[10])
  expect_true(memberships[["TFDD_MEM"]]$Beg[50] <
                memberships[["TFDD_MEM"]]$Beg[75])
  expect_true(memberships[["TFDD_MEM"]]$Beg[100] <
                memberships[["TFDD_MEM"]]$Beg[120])
})
