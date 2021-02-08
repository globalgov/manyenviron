# Test if  meets the q ecosystem requirements

# Requires the following package
library(pointblank)

# # Ensure the dataset is in tibble format
# test_that("exported data is in tibble format", {
#   expect_message(tibble::is_tibble("IEADB"), "TRUE")
# })

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^.$", agreements[["IEADB"]])))
  expect_false(any(grepl("^n/a$", agreements[["IEADB"]])))
  expect_false(any(grepl("^N/A$", agreements[["IEADB"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the correct variables", {
  expect_col_exists(agreements[["IEADB"]], vars(Title))
  expect_col_exists(agreements[["IEADB"]], vars(Signature))
  expect_col_exists(agreements[["IEADB"]], vars(Force))
})

# Dates are standardized
test_that("dates are standardised", {
  expect_col_is_date(agreements[["IEADB"]], vars(Signature))
  expect_col_is_date(agreements[["IEADB"]], vars(Force))
  expect_false(any(grepl("/", agreements[["IEADB"]]$Signature)))
  expect_false(any(grepl("/", agreements[["IEADB"]]$Force)))
})
