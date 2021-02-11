# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", agreements[["GNEVAR"]])))
  expect_false(any(grepl("^n/a$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("^N/A$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("^\\s$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("^\\.$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["GNEVAR"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the correct variables", {
  expect_col_exists(agreements[["GNEVAR"]], vars(Title))
  expect_col_exists(agreements[["GNEVAR"]], vars(Beg))
})

# Dates are standardized for mandatory column
test_that("dates are standardised", {
  expect_col_is_date(agreements[["GNEVAR"]], vars(Beg))
  expect_false(any(grepl("/", agreements[["GNEVAR"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["GNEVAR"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["GNEVAR"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$", agreements[["GNEVAR"]]$Beg)))
})

# Dates are standardized for optional columns
test_that("Columns with dates are standardized", {
  if(!is.null(agreements[["GNEVAR"]]$End)) {
    expect_false(any(grepl("/", agreements[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["GNEVAR"]]$End)))
  }
  if (!is.null(agreements[["GNEVAR"]]$Force)) {
    expect_false(any(grepl("/", agreements[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["GNEVAR"]]$Force)))
  }
  if (!is.null(agreements[["GNEVAR"]]$Rat)) {
    expect_false(any(grepl("/", agreements[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["GNEVAR"]]$Rat)))
  }
})

