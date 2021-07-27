# Test if  meets the q ecosystem requirements

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
  expect_col_exists(agreements[["IEADB"]], vars(Title))
  expect_col_exists(agreements[["IEADB"]], vars(Beg))
  expect_true(any(grepl("ID$", colnames(agreements[["IEADB"]]))))
  expect_col_exists(agreements[["IEADB"]], vars(Signature))
  expect_col_exists(agreements[["IEADB"]], vars(Force))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(lubridate::is.Date(agreements[["IEADB"]]))
  expect_false(lubridate::is.POSIXct(agreements[["IEADB"]]))
  expect_false(lubridate::is.POSIXlt(agreements[["IEADB"]]))
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
                         agreements[["IEADB"]]$Sighature)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["IEADB"]]$Signature)))
})

test_that("Column `Force` has standardised dates", {
  expect_equal(class(agreements[["IEADB"]]$Force), "messydt")
  expect_false(any(grepl("/", agreements[["IEADB"]]$Force)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["IEADB"]]$Force)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         agreements[["IEADB"]]$Force)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         agreements[["IEADB"]]$Force)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         agreements[["IEADB"]]$Force)))
})

# Dates are standardized for optional columns
test_that("Columns with dates are standardized", {
  if (!is.null(agreements[["IEADB"]]$End)) {
    expect_false(any(grepl("/", agreements[["IEADB"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           agreements[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           agreements[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           agreements[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           agreements[["IEADB"]]$End)))
  }
  if (!is.null(agreements[["IEADB"]]$Rat)) {
    expect_false(any(grepl("/", agreements[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           agreements[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           agreements[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           agreements[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           agreements[["IEADB"]]$Rat)))
  }
  if (!is.null(agreements[["IEADB"]]$Term)) {
    expect_equal(class(agreements[["IEADB"]]$Term), "messydt")
    expect_false(any(grepl("/", agreements[["IEADB"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           agreements[["IEADB"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           agreements[["IEADB"]]$Term)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           agreements[["IEADB"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           agreements[["IEADB"]]$Term)))
  }
})

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["IEADB"]]$Beg[1] < agreements[["IEADB"]]$Beg[10])
  expect_true(agreements[["IEADB"]]$Beg[50] < agreements[["IEADB"]]$Beg[75])
  expect_true(agreements[["IEADB"]]$Beg[100] < agreements[["IEADB"]]$Beg[120])
})
