# Test if  meets the q ecosystem requirements

# Requires the following package
library(pointblank)

# Report missing values 
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^.$", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("^n/a$", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("^N/A$", memberships[["IEA_MEM"]])))
})

# Labels are standardized
test_that("labels are standardised", {
  expect_false(any(grepl("U.S.", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("U.K.", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("!", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("NANA.", memberships[["IEA_MEM"]])))
})
