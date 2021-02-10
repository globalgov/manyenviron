# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", agreements[["IEADB"]])))
  expect_false(any(grepl("^n/a$", agreements[["IEADB"]])))
  expect_false(any(grepl("^N/A$", agreements[["IEADB"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the correct variables", {
  expect_col_exists(agreements[["IEADB"]], vars(Title))
  expect_col_exists(agreements[["IEADB"]], vars(Beg))
})

# Dates are standardized
test_that("dates are standardised", {
  expect_col_is_date(agreements[["IEADB"]], vars(Beg))
  expect_false(any(grepl("/", agreements[["IEADB"]]$Beg)))
  expect_false(any(grepl("\\?", agreements[["IEADB"]]$Beg)))
})
