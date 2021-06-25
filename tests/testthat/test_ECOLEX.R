# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", memberships[["ECOLEX"]])))
  expect_false(any(grepl("^n/a$", memberships[["ECOLEX"]])))
  expect_false(any(grepl("^N/A$", memberships[["ECOLEX"]])))
  expect_false(any(grepl("^\\s$", memberships[["ECOLEX"]])))
  expect_false(any(grepl("^\\.$", memberships[["ECOLEX"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["ECOLEX"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["ECOLEX"]])))
})

# At least one column named ID
test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("_ID$", colnames(memberships[["ECOLEX"]]))))
})

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null(memberships[["ECOLEX"]]$Label)) {
  expect_false(any(grepl("U.S.", memberships[["ECOLEX"]])))
  expect_false(any(grepl("U.K.", memberships[["ECOLEX"]])))
  expect_false(any(grepl("!", memberships[["ECOLEX"]])))
  expect_false(any(grepl("NANA.", memberships[["ECOLEX"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null(memberships[["ECOLEX"]]$Beg)) {
    expect_false(any(grepl("/", memberships[["ECOLEX"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX"]]$Beg)))
  }
  if (!is.null(memberships[["ECOLEX"]]$End)) {
    expect_false(any(grepl("/", memberships[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX"]]$End)))
  }
  if (!is.null(memberships[["ECOLEX"]]$Force)) {
    expect_false(any(grepl("/", memberships[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX"]]$Force)))
  }
  if (!is.null(memberships[["ECOLEX"]]$Rat)) {
    expect_false(any(grepl("/", memberships[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX"]]$Rat)))
  }
  if (!is.null(memberships[["ECOLEX"]]$Signature)) {
    expect_false(any(grepl("/", memberships[["ECOLEX"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX"]]$Signature)))
  }
  if (!is.null(memberships[["ECOLEX"]]$Term)) {
    expect_false(any(grepl("/", memberships[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX"]]$Term)))
  }
  if (!is.null(memberships[["ECOLEX"]]$Withdrawal)) {
    expect_false(any(grepl("/", memberships[["ECOLEX"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["ECOLEX"]]$Withdrawal)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX"]]$Withdrawal)))
  }
})
