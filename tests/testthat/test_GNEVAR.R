# Test if  meets the q ecosystem requirements

# Requires the following package
library(pointblank)

# # Ensure the dataset is in tibble format
# test_that("exported data is in tibble format", {
#   expect_message(tibble::is_tibble("GNEVAR"), "TRUE")
# })

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^.$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("^n/a$", agreements[["GNEVAR"]])))
  expect_false(any(grepl("^N/A$", agreements[["GNEVAR"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the correct variables", {
  expect_col_exists(agreements[["GNEVAR"]], vars(Title))
  expect_col_exists(agreements[["GNEVAR"]], vars(Signature))
  expect_col_exists(agreements[["GNEVAR"]], vars(Force))
})

# Dates are standardized
test_that("dates are standardised", {
  expect_col_is_date(agreements[["GNEVAR"]], vars(Signature))
  expect_col_is_date(agreements[["GNEVAR"]], vars(Force))
  expect_false(any(grepl("/", agreements[["GNEVAR"]]$Signature)))
  expect_false(any(grepl("/", agreements[["GNEVAR"]]$Force)))
})
