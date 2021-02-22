# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("^n/a$", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("^N/A$", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("^\\s$", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("^\\.$", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["IEA_MEM"]])))
})

# At least one column named ID 
test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("_ID$", colnames(memberships[["IEA_MEM"]]))))
})

# Labels are standardized
test_that("labels are standardised", {
  if(!is.null(memberships[["IEA_MEM"]]$Label)) {
  expect_false(any(grepl("U.S.", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("U.K.", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("!", memberships[["IEA_MEM"]])))
  expect_false(any(grepl("NANA.", memberships[["IEA_MEM"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if(!is.null(memberships[["IEA_MEM"]]$Beg)) {
    expect_false(any(grepl("/", memberships[["IEA_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["IEA_MEM"]]$Beg)))
  } 
  if(!is.null(memberships[["IEA_MEM"]]$End)) {
    expect_false(any(grepl("/", memberships[["IEA_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$End)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["IEA_MEM"]]$End)))
  }
  if (!is.null(memberships[["IEA_MEM"]]$Force)) {
    expect_false(any(grepl("/", memberships[["IEA_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["IEA_MEM"]]$Force)))
  }
  if (!is.null(memberships[["IEA_MEM"]]$Rat)) {
    expect_false(any(grepl("/", memberships[["IEA_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["IEA_MEM"]]$Rat)))
  }
  if (!is.null(memberships[["IEA_MEM"]]$Signature)) {
    expect_false(any(grepl("/", memberships[["IEA_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["IEA_MEM"]]$Signature)))
  }
  if (!is.null(memberships[["IEA_MEM"]]$Term)) {
    expect_false(any(grepl("/", memberships[["IEA_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["IEA_MEM"]]$Term)))
  }
  if (!is.null(memberships[["IEA_MEM"]]$Withdrawal)) {
    expect_false(any(grepl("/", memberships[["IEA_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$", memberships[["IEA_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:alpha:]$", memberships[["IEA_MEM"]]$Withdrawal)))
  }
})
