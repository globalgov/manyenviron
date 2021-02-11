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
test_that("datasets have the correct variables", {
  expect_col_exists(agreements[["ECOLEX"]], vars(Title))
  expect_col_exists(agreements[["ECOLEX"]], vars(Beg))
})

# Dates are standardized for mandatory column
test_that("dates are standardised", {
  expect_col_is_date(agreements[["ECOLEX"]], vars(Beg))
  expect_false(any(grepl("/", agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$", agreements[["ECOLEX"]]$Beg)))
})

# Dates are standardized for optional columns
test_that("Columns with dates are standardized", {
  if(!is.null(agreements[["ECOLEX"]]$End)) {
    expect_false(any(grepl("/", agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["ECOLEX"]]$End)))
  }
  if (!is.null(agreements[["ECOLEX"]]$Force)) {
    expect_false(any(grepl("/", agreements[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["ECOLEX"]]$Force)))
  }
  if (!is.null(agreements[["ECOLEX"]]$Rat)) {
    expect_false(any(grepl("/", agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["ECOLEX"]]$Rat)))
  }
})

