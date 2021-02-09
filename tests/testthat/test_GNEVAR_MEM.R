# Test if  meets the q ecosystem requirements
library(pointblank)

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("^n/a$", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("^N/A$", memberships[["GNEVAR_MEM"]])))
})

# Labels are standardized
test_that("labels are standardised", {
  # expect_false(any(grepl("U.S.", memberships[["GNEVAR_MEM"]])))
  # expect_false(any(grepl("U.K.", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("!", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("NANA.", memberships[["GNEVAR_MEM"]])))
})
