# Test if  meets the q ecosystem requirements

# Requires the following package
library(pointblank)

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^.$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^n/a$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^N/A$", memberships[["ECOLEX_MEM"]])))
})

# Labels are standardized
test_that("labels are standardised", {
  expect_false(any(grepl("U.S.", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("U.K.", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("!", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("NANA.", memberships[["ECOLEX_MEM"]])))
})
