# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("^n/a$", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("^N/A$", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("^\\s$", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("^\\.$", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["GNEVAR_MEM"]])))
})

# Labels are standardized
test_that("labels are standardised", {
  if(!is.null(memberships[["GNEVAR_MEM"]]$Label)) {
  expect_false(any(grepl("U.S.", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("U.K.", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("!", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("NANA.", memberships[["GNEVAR_MEM"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if(!is.null(memberships[["GNEVAR_MEM"]]$Beg)) {
    expect_false(any(grepl("/", memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["GNEVAR_MEM"]]$Beg)))
  } 
  if(!is.null(memberships[["GNEVAR_MEM"]]$End)) {
    expect_false(any(grepl("/", memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["GNEVAR_MEM"]]$End)))
  }
  if (!is.null(memberships[["GNEVAR_MEM"]]$Force)) {
    expect_false(any(grepl("/", memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["GNEVAR_MEM"]]$Force)))
  }
  if (!is.null(memberships[["GNEVAR_MEM"]]$Rat)) {
    expect_false(any(grepl("/", memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["GNEVAR_MEM"]]$Rat)))
  }
})
