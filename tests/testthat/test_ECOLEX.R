# Test if  meets the q ecosystem requirements
library(pointblank)

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", agreements[["ECOLEX"]])))
  expect_false(any(grepl("^n/a$", agreements[["ECOLEX"]])))
  expect_false(any(grepl("^N/A$", agreements[["ECOLEX"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the correct variables", {
  expect_col_exists(agreements[["ECOLEX"]], vars(Title))
  expect_col_exists(agreements[["ECOLEX"]], vars(Beg))
})

# Dates are standardized
test_that("dates are standardised", {
  expect_col_is_date(agreements[["ECOLEX"]], vars(Beg))
  expect_false(any(grepl("/", agreements[["ECOLEX"]]$Beg)))
  expect_false(any(grepl("\\?", agreements[["ECOLEX"]]$Beg)))
})
