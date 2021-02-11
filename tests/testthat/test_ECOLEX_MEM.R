# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^n/a$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^N/A$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^\\s$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^\\.$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["ECOLEX_MEM"]])))
})

# Labels are standardized
test_that("labels are standardised", {
  if(!is.null(memberships[["ECOLEX_MEM"]]$Label)) {
  expect_false(any(grepl("U.S.", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("U.K.", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("!", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("NANA.", memberships[["ECOLEX_MEM"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if(!is.null(memberships[["ECOLEX_MEM"]]$Beg)) {
    expect_false(any(grepl("/", memberships[["ECOLEX_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["ECOLEX_MEM"]]$Beg)))
  } 
  if(!is.null(memberships[["ECOLEX_MEM"]]$End)) {
    expect_false(any(grepl("/", memberships[["ECOLEX_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$End)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["ECOLEX_MEM"]]$End)))
  }
  if (!is.null(memberships[["ECOLEX_MEM"]]$Force)) {
    expect_false(any(grepl("/", memberships[["ECOLEX_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["ECOLEX_MEM"]]$Force)))
  }
  if (!is.null(memberships[["ECOLEX_MEM"]]$Rat)) {
    expect_false(any(grepl("/", memberships[["ECOLEX_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["ECOLEX_MEM"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["ECOLEX_MEM"]]$Rat)))
  }
})
