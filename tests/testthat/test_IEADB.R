# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", memberships[["IEADB"]])))
  expect_false(any(grepl("^n/a$", memberships[["IEADB"]])))
  expect_false(any(grepl("^N/A$", memberships[["IEADB"]])))
  expect_false(any(grepl("^\\s$", memberships[["IEADB"]])))
  expect_false(any(grepl("^\\.$", memberships[["IEADB"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["IEADB"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["IEADB"]])))
})

# At least one column named ID
test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("_ID$", colnames(memberships[["IEADB"]]))))
})

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null(memberships[["IEADB"]]$Label)) {
  expect_false(any(grepl("U.S.", memberships[["IEADB"]])))
  expect_false(any(grepl("U.K.", memberships[["IEADB"]])))
  expect_false(any(grepl("!", memberships[["IEADB"]])))
  expect_false(any(grepl("NANA.", memberships[["IEADB"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null(memberships[["IEADB"]]$Beg)) {
    expect_false(any(grepl("/", memberships[["IEADB"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["IEADB"]]$Beg)))
  }
  if (!is.null(memberships[["IEADB"]]$End)) {
    expect_false(any(grepl("/", memberships[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["IEADB"]]$End)))
  }
  if (!is.null(memberships[["IEADB"]]$Force)) {
    expect_false(any(grepl("/", memberships[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["IEADB"]]$Force)))
  }
  if (!is.null(memberships[["IEADB"]]$Rat)) {
    expect_false(any(grepl("/", memberships[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["IEADB"]]$Rat)))
  }
  if (!is.null(memberships[["IEADB"]]$Signature)) {
    expect_false(any(grepl("/", memberships[["IEADB"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["IEADB"]]$Signature)))
  }
  if (!is.null(memberships[["IEADB"]]$Term)) {
    expect_false(any(grepl("/", memberships[["IEADB"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["IEADB"]]$Term)))
  }
  if (!is.null(memberships[["IEADB"]]$Withdrawal)) {
    expect_false(any(grepl("/", memberships[["IEADB"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["IEADB"]]$Withdrawal)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["IEADB"]]$Withdrawal)))
  }
})
