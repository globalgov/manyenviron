# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", agreements[["ECOLEX"]])))
  expect_false(any(grepl("^n/a$", agreements[["ECOLEX"]])))
  expect_false(any(grepl("^N/A$", agreements[["ECOLEX"]])))
  expect_false(any(grepl("^\\s$", agreements[["ECOLEX"]])))
  expect_false(any(grepl("^\\.$", agreements[["ECOLEX"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["ECOLEX"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["ECOLEX"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  expect_col_exists(agreements[["ECOLEX"]], vars(Title))
  expect_col_exists(agreements[["ECOLEX"]], vars(Beg))
  expect_true(any(grepl("_ID$", colnames(agreements[["ECOLEX"]]))))
  expect_col_exists(agreements[["ECOLEX"]], vars(Signature))
  expect_col_exists(agreements[["ECOLEX"]], vars(Force))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_col_is_date(agreements[["ECOLEX"]], vars(Beg))
  expect_false(any(grepl("/", agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["ECOLEX"]]$Beg)))
})

test_that("Column `Signature` has standardised dates", {
  expect_col_is_date(agreements[["ECOLEX"]], vars(Signature))
  expect_false(any(grepl("/", agreements[["ECOLEX"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Signature)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Signature)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["ECOLEX"]]$Signature)))
})

test_that("Column `Force` has standardised dates", {
  expect_col_is_date(agreements[["ECOLEX"]], vars(Force))
  expect_false(any(grepl("/", agreements[["ECOLEX"]]$Force)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Force)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Force)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Force)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Force)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                         agreements[["ECOLEX"]]$Force)))
  expect_false(any(grepl("^[:alpha:]$",
                         agreements[["ECOLEX"]]$Force)))
})

# Dates are standardized for optional columns
test_that("Columns with dates are standardized", {
  if (!is.null(agreements[["ECOLEX"]]$End)) {
    expect_false(any(grepl("/", agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           agreements[["ECOLEX"]]$End)))
  }
  if (!is.null(agreements[["ECOLEX"]]$Rat)) {
    expect_false(any(grepl("/", agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           agreements[["ECOLEX"]]$Rat)))
  }
  if (!is.null(agreements[["ECOLEX"]]$Term)) {
    expect_false(any(grepl("/", agreements[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           agreements[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           agreements[["ECOLEX"]]$Term)))
  }
})

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by date variable", {
  expect_true(agreements[["ECOLEX"]]$Beg[1] < agreements[["ECOLEX"]]$Beg[10])
  expect_true(agreements[["ECOLEX"]]$Beg[50] < agreements[["ECOLEX"]]$Beg[75])
  expect_true(agreements[["ECOLEX"]]$Beg[100] < agreements[["ECOLEX"]]$Beg[120])
})
