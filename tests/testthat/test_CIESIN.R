# Test if  meets the q ecosystem requirements

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
  expect_col_exists(agreements[["CIESIN"]], vars(Title))
  expect_col_exists(agreements[["CIESIN"]], vars(Beg))
  expect_true(any(grepl("ID$", colnames(agreements[["CIESIN"]]))))
  expect_col_exists(agreements[["CIESIN"]], vars(Signature))
  expect_col_exists(agreements[["CIESIN"]], vars(Force))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(lubridate::is.Date(agreements[["CIESIN"]]))
  expect_false(lubridate::is.POSIXct(agreements[["CIESIN"]]))
  expect_false(lubridate::is.POSIXlt(agreements[["CIESIN"]]))
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
                         agreements[["CIESIN"]]$Sighature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["CIESIN"]]$Signature)))
})

test_that("Column `Force` has standardised dates", {
  expect_equal(class(agreements[["CIESIN"]]$Force), "messydt")
  expect_false(any(grepl("/", agreements[["CIESIN"]]$Force)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["CIESIN"]]$Force)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["CIESIN"]]$Force)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["CIESIN"]]$Force)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["CIESIN"]]$Force)))
})

# Dates are standardized for optional columns
test_that("Columns with dates are standardized", {
  if (!is.null(agreements[["CIESIN"]]$End)) {
    expect_false(any(grepl("/", agreements[["CIESIN"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           agreements[["CIESIN"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           agreements[["CIESIN"]]$End)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           agreements[["CIESIN"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           agreements[["CIESIN"]]$End)))
  }
  if (!is.null(agreements[["CIESIN"]]$Rat)) {
    expect_false(any(grepl("/", agreements[["CIESIN"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           agreements[["CIESIN"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           agreements[["CIESIN"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           agreements[["CIESIN"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           agreements[["CIESIN"]]$Rat)))
  }
  if (!is.null(agreements[["CIESIN"]]$Term)) {
    expect_equal(class(agreements[["CIESIN"]]$Term), "messydt")
    expect_false(any(grepl("/", agreements[["CIESIN"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           agreements[["CIESIN"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           agreements[["CIESIN"]]$Term)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           agreements[["CIESIN"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           agreements[["CIESIN"]]$Term)))
  }
})

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["CIESIN"]]$Beg[1] < agreements[["CIESIN"]]$Beg[10])
  expect_true(agreements[["CIESIN"]]$Beg[50] < agreements[["CIESIN"]]$Beg[75])
  expect_true(agreements[["CIESIN"]]$Beg[100] < agreements[["CIESIN"]]$Beg[120])
})
