# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("\\?", references[["REF"]])))
  expect_false(any(grepl("^n/a$", references[["REF"]])))
  expect_false(any(grepl("^N/A$", references[["REF"]])))
  expect_false(any(grepl("^\\s$", references[["REF"]])))
  expect_false(any(grepl("^\\.$", references[["REF"]])))
  expect_false(any(grepl("N\\.A\\.$", references[["REF"]])))
  expect_false(any(grepl("n\\.a\\.$", references[["REF"]])))
})

# At least one column named ID
test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("_ID$", colnames(references[["REF"]]))))
})

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null(references[["REF"]]$Label)) {
  expect_false(any(grepl("U.S.", references[["REF"]])))
  expect_false(any(grepl("U.K.", references[["REF"]])))
  expect_false(any(grepl("!", references[["REF"]])))
  expect_false(any(grepl("NANA.", references[["REF"]])))
  }
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null(references[["REF"]]$Beg)) {
    expect_equal(class(references[["REF"]]$Beg), "messydt")
    expect_false(any(grepl("/", references[["REF"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           references[["REF"]]$Beg)))
  }
  if (!is.null(references[["REF"]]$End)) {
    expect_equal(class(references[["REF"]]$End), "messydt")
    expect_false(any(grepl("/", references[["REF"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           references[["REF"]]$End)))
  }
  if (!is.null(references[["REF"]]$Force)) {
    expect_equal(class(references[["REF"]]$Force), "messydt")
    expect_false(any(grepl("/", references[["REF"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           references[["REF"]]$Force)))
  }
  if (!is.null(references[["REF"]]$Rat)) {
    expect_equal(class(references[["REF"]]$Rat), "messydt")
    expect_false(any(grepl("/", references[["REF"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           references[["REF"]]$Rat)))
  }
  if (!is.null(references[["REF"]]$Signature)) {
    expect_equal(class(references[["REF"]]$Signature), "messydt")
    expect_false(any(grepl("/", references[["REF"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           references[["REF"]]$Signature)))
  }
  if (!is.null(references[["REF"]]$Term)) {
    expect_equal(class(references[["REF"]]$Term), "messydt")
    expect_false(any(grepl("/", references[["REF"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Term)))
    expect_false(any(grepl("^[:alpha:]$",
                           references[["REF"]]$Term)))
  }
  if (!is.null(references[["REF"]]$Withdrawal)) {
    expect_equal(class(references[["REF"]]$Withdrawal), "messydt")
    expect_false(any(grepl("/", references[["REF"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{1}-[:digit:]{4}$",
                           references[["REF"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{1}-[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Withdrawal)))
    expect_false(any(grepl("^[:digit:]{2}-[:digit:]{4}$",
                           references[["REF"]]$Withdrawal)))
    expect_false(any(grepl("^[:alpha:]$",
                           references[["REF"]]$Withdrawal)))
  }
})

# Dataset should be ordered according to the "Beg" column
# if the column exists
  test_that("dataset is arranged by date variable", {
    if (!is.null(references[["REF"]]$Beg)) {
  expect_true(references[["REF"]]$Beg[1] < references[["REF"]]$Beg[10])
  expect_true(references[["REF"]]$Beg[50] < references[["REF"]]$Beg[75])
  expect_true(references[["REF"]]$Beg[100] < references[["REF"]]$Beg[120])
    }
})
