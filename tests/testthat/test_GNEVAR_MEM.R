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

# At least one column named ID
test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("_ID$", colnames(memberships[["GNEVAR_MEM"]]))))
})

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null(memberships[["GNEVAR_MEM"]]$Label)) {
  expect_false(any(grepl("U.S.", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("U.K.", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("!", memberships[["GNEVAR_MEM"]])))
  expect_false(any(grepl("NANA.", memberships[["GNEVAR_MEM"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null(memberships[["GNEVAR_MEM"]]$Beg)) {
    expect_equal(class(memberships[["GNEVAR_MEM"]]$Beg), "messydt")
    expect_false(any(grepl("/", memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR_MEM"]]$Beg)))
  }
  if (!is.null(memberships[["GNEVAR_MEM"]]$End)) {
    expect_equal(class(memberships[["GNEVAR_MEM"]]$End), "messydt")
    expect_false(any(grepl("/", memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR_MEM"]]$End)))
  }
  if (!is.null(memberships[["GNEVAR_MEM"]]$Force)) {
    expect_equal(class(memberships[["GNEVAR_MEM"]]$Force), "messydt")
    expect_false(any(grepl("/", memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR_MEM"]]$Force)))
  }
  if (!is.null(memberships[["GNEVAR_MEM"]]$Rat)) {
    expect_equal(class(memberships[["GNEVAR_MEM"]]$Rat), "messydt")
    expect_false(any(grepl("/", memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR_MEM"]]$Rat)))
  }
  if (!is.null(memberships[["GNEVAR_MEM"]]$Signature)) {
    expect_equal(class(memberships[["GNEVAR_MEM"]]$Signature), "messydt")
    expect_false(any(grepl("/", memberships[["GNEVAR_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR_MEM"]]$Signature)))
  }
  if (!is.null(memberships[["GNEVAR_MEM"]]$Term)) {
    expect_equal(class(memberships[["GNEVAR_MEM"]]$Term), "messydt")
    expect_false(any(grepl("/", memberships[["GNEVAR_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR_MEM"]]$Term)))
  }
  if (!is.null(memberships[["GNEVAR_MEM"]]$Withdrawal)) {
    expect_equal(class(memberships[["GNEVAR_MEM"]]$Withdrawal), "messydt")
    expect_false(any(grepl("/", memberships[["GNEVAR_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           memberships[["GNEVAR_MEM"]]$Withdrawal)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["GNEVAR_MEM"]]$Withdrawal)))
  }
})

# Dataset should be ordered according to the "Beg" column
# if the column exists
  test_that("dataset is arranged by date variable", {
    if (!is.null(memberships[["GNEVAR_MEM"]]$Beg)) {
  expect_true(memberships[["GNEVAR_MEM"]]$Beg[1] < memberships[["GNEVAR_MEM"]]$Beg[10])
  expect_true(memberships[["GNEVAR_MEM"]]$Beg[50] < memberships[["GNEVAR_MEM"]]$Beg[75])
  expect_true(memberships[["GNEVAR_MEM"]]$Beg[100] < memberships[["GNEVAR_MEM"]]$Beg[120])
    }
})
