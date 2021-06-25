# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", memberships[["GNEVAR"]])))
  expect_false(any(grepl("^n/a$", memberships[["GNEVAR"]])))
  expect_false(any(grepl("^N/A$", memberships[["GNEVAR"]])))
  expect_false(any(grepl("^\\s$", memberships[["GNEVAR"]])))
  expect_false(any(grepl("^\\.$", memberships[["GNEVAR"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["GNEVAR"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["GNEVAR"]])))
})

# At least one column named ID
test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("_ID$", colnames(memberships[["GNEVAR"]]))))
})

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null(memberships[["GNEVAR"]]$Label)) {
  expect_false(any(grepl("U.S.", memberships[["GNEVAR"]])))
  expect_false(any(grepl("U.K.", memberships[["GNEVAR"]])))
  expect_false(any(grepl("!", memberships[["GNEVAR"]])))
  expect_false(any(grepl("NANA.", memberships[["GNEVAR"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null(memberships[["GNEVAR"]]$Beg)) {
    expect_false(any(grepl("/", memberships[["GNEVAR"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR"]]$Beg)))
  }
  if (!is.null(memberships[["GNEVAR"]]$End)) {
    expect_false(any(grepl("/", memberships[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR"]]$End)))
  }
  if (!is.null(memberships[["GNEVAR"]]$Force)) {
    expect_false(any(grepl("/", memberships[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR"]]$Force)))
  }
  if (!is.null(memberships[["GNEVAR"]]$Rat)) {
    expect_false(any(grepl("/", memberships[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR"]]$Rat)))
  }
  if (!is.null(memberships[["GNEVAR"]]$Signature)) {
    expect_false(any(grepl("/", memberships[["GNEVAR"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR"]]$Signature)))
  }
  if (!is.null(memberships[["GNEVAR"]]$Term)) {
    expect_false(any(grepl("/", memberships[["GNEVAR"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR"]]$Term)))
  }
  if (!is.null(memberships[["GNEVAR"]]$Withdrawal)) {
    expect_false(any(grepl("/", memberships[["GNEVAR"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR"]]$Withdrawal)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR"]]$Withdrawal)))
  }
})
