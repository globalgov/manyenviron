# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", agreements[["CIESIN"]])))
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
  expect_true(any(grepl("_ID$", colnames(agreements[["CIESIN"]]))))
  expect_col_exists(agreements[["CIESIN"]], vars(Signature))
  expect_col_exists(agreements[["CIESIN"]], vars(Force))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_col_is_date(agreements[["CIESIN"]], vars(Beg))
  expect_false(any(grepl("/", agreements[["CIESIN"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$", agreements[["CIESIN"]]$Beg)))
})

test_that("Column `Signature` has standardised dates", {
  expect_col_is_date(agreements[["CIESIN"]], vars(Signature))
  expect_false(any(grepl("/", agreements[["CIESIN"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$", agreements[["CIESIN"]]$Signature)))
})

test_that("Column `Force` has standardised dates", {
  expect_col_is_date(agreements[["CIESIN"]], vars(Force))
  expect_false(any(grepl("/", agreements[["CIESIN"]]$Force)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Force)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$Force)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$Force)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Force)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Force)))
  expect_false(any(grepl("^[:alpha:]$", agreements[["CIESIN"]]$Force)))
})

# Dates are standardized for optional columns
test_that("Columns with dates are standardized", {
  if(!is.null(agreements[["CIESIN"]]$End)) {
    expect_false(any(grepl("/", agreements[["CIESIN"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$End)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["CIESIN"]]$End)))
  }
  if (!is.null(agreements[["CIESIN"]]$Rat)) {
    expect_false(any(grepl("/", agreements[["CIESIN"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["CIESIN"]]$Rat)))
  }
  if (!is.null(agreements[["CIESIN"]]$Term)) {
    expect_false(any(grepl("/", agreements[["CIESIN"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["CIESIN"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["CIESIN"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["CIESIN"]]$Term)))
  }
})
