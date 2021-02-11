# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", agreements[["IEADB"]])))
  expect_false(any(grepl("^n/a$", agreements[["IEADB"]])))
  expect_false(any(grepl("^N/A$", agreements[["IEADB"]])))
  expect_false(any(grepl("^\\s$", agreements[["IEADB"]])))
  expect_false(any(grepl("^\\.$", agreements[["IEADB"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["IEADB"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["IEADB"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the correct variables", {
  expect_col_exists(agreements[["IEADB"]], vars(Title))
  expect_col_exists(agreements[["IEADB"]], vars(Beg))
})

# Dates are standardized for mandatory column
test_that("dates are standardised", {
  expect_col_is_date(agreements[["IEADB"]], vars(Beg))
  expect_false(any(grepl("/", agreements[["IEADB"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["IEADB"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["IEADB"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$", agreements[["IEADB"]]$Beg)))
})

# Dates are standardized for optional columns
test_that("Columns with dates are standardized", {
  if(!is.null(agreements[["IEADB"]]$End)) {
    expect_false(any(grepl("/", agreements[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$End)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["IEADB"]]$End)))
  }
  if (!is.null(agreements[["IEADB"]]$Force)) {
    expect_false(any(grepl("/", agreements[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["IEADB"]]$Force)))
  }
  if (!is.null(agreements[["IEADB"]]$Rat)) {
    expect_false(any(grepl("/", agreements[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", agreements[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", agreements[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", agreements[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$", agreements[["IEADB"]]$Rat)))
  }
})

