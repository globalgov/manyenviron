# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", memberships[["TFDD"]])))
  expect_false(any(grepl("^n/a$", memberships[["TFDD"]])))
  expect_false(any(grepl("^N/A$", memberships[["TFDD"]])))
  expect_false(any(grepl("^\\s$", memberships[["TFDD"]])))
  expect_false(any(grepl("^\\.$", memberships[["TFDD"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["TFDD"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["TFDD"]])))
})

# At least one column named ID
test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("_ID$", colnames(memberships[["TFDD"]]))))
})

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null(memberships[["TFDD"]]$Label)) {
  expect_false(any(grepl("U.S.", memberships[["TFDD"]])))
  expect_false(any(grepl("U.K.", memberships[["TFDD"]])))
  expect_false(any(grepl("!", memberships[["TFDD"]])))
  expect_false(any(grepl("NANA.", memberships[["TFDD"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null(memberships[["TFDD"]]$Beg)) {
    expect_false(any(grepl("/", memberships[["TFDD"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["TFDD"]]$Beg)))
  }
  if (!is.null(memberships[["TFDD"]]$End)) {
    expect_false(any(grepl("/", memberships[["TFDD"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["TFDD"]]$End)))
  }
  if (!is.null(memberships[["TFDD"]]$Force)) {
    expect_false(any(grepl("/", memberships[["TFDD"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["TFDD"]]$Force)))
  }
  if (!is.null(memberships[["TFDD"]]$Rat)) {
    expect_false(any(grepl("/", memberships[["TFDD"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["TFDD"]]$Rat)))
  }
  if (!is.null(memberships[["TFDD"]]$Signature)) {
    expect_false(any(grepl("/", memberships[["TFDD"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["TFDD"]]$Signature)))
  }
  if (!is.null(memberships[["TFDD"]]$Term)) {
    expect_false(any(grepl("/", memberships[["TFDD"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["TFDD"]]$Term)))
  }
  if (!is.null(memberships[["TFDD"]]$Withdrawal)) {
    expect_false(any(grepl("/", memberships[["TFDD"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["TFDD"]]$Withdrawal)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["TFDD"]]$Withdrawal)))
  }
})
