# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^N/A$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^\\s$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("^\\.$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("N\\.A\\.$", memberships[["ECOLEX_MEM"]])))
  expect_false(any(grepl("n\\.a\\.$", memberships[["ECOLEX_MEM"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  expect_col_exists(memberships[["ECOLEX_MEM"]], vars(Country))
  expect_col_exists(memberships[["ECOLEX_MEM"]], vars(Beg))
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(lubridate::is.Date(memberships[["ECOLEX_MEM"]]))
  expect_false(lubridate::is.POSIXct(memberships[["ECOLEX_MEM"]]))
  expect_false(lubridate::is.POSIXlt(memberships[["ECOLEX_MEM"]]))
})

# Dates are standardized for mandatory column
test_that("Column `Beg` has standardised dates", {
  expect_equal(class(memberships[["ECOLEX_MEM"]]$Beg), "messydt")
  expect_false(any(grepl("/", memberships[["ECOLEX_MEM"]]$Beg)))
  expect_false(any(grepl("^[:alpha:]$",
                         memberships[["ECOLEX_MEM"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{2}$",
                         memberships[["ECOLEX_MEM"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{3}$",
                         memberships[["ECOLEX_MEM"]]$Beg)))
  expect_false(any(grepl("^[:digit:]{1}$",
                         memberships[["ECOLEX_MEM"]]$Beg)))
})

# Dates are standardized for optional columns
test_that("Optional dates columns are standardized", {
  if (!is.null(memberships[["ECOLEX_MEM"]]$Force)) {
    expect_equal(class(memberships[["ECOLEX_MEM"]]$Force), "messydt")
    expect_false(any(grepl("/", memberships[["ECOLEX_MEM"]]$Force)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           memberships[["ECOLEX_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           memberships[["ECOLEX_MEM"]]$Force)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           memberships[["ECOLEX_MEM"]]$Force)))
  }
  if (!is.null(memberships[["ECOLEX_MEM"]]$Signature)) {
    expect_equal(class(memberships[["ECOLEX_MEM"]]$Signature), "messydt")
    expect_false(any(grepl("/", memberships[["ECOLEX_MEM"]]$Signature)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           memberships[["ECOLEX_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           memberships[["ECOLEX_MEM"]]$Signature)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           memberships[["ECOLEX_MEM"]]$Signature)))
  }
  if (!is.null(memberships[["ECOLEX_MEM"]]$End)) {
    expect_false(any(grepl("/", memberships[["ECOLEX_MEM"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           memberships[["ECOLEX_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           memberships[["ECOLEX_MEM"]]$End)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           memberships[["ECOLEX_MEM"]]$End)))
  }
  if (!is.null(memberships[["ECOLEX_MEM"]]$Rat)) {
    expect_false(any(grepl("/", memberships[["ECOLEX_MEM"]]$Rat)))
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           memberships[["ECOLEX_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           memberships[["ECOLEX_MEM"]]$Rat)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           memberships[["ECOLEX_MEM"]]$Rat)))
  }
  if (!is.null(memberships[["ECOLEX_MEM"]]$Term)) {
    expect_equal(class(memberships[["ECOLEX_MEM"]]$Term), "messydt")
    expect_false(any(grepl("^[:alpha:]$",
                           memberships[["ECOLEX_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           memberships[["ECOLEX_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           memberships[["ECOLEX_MEM"]]$Term)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           memberships[["ECOLEX_MEM"]]$Term)))
  }
})

# Dataset should be ordered according to the "Beg" column
test_that("dataset is arranged by the `Beg` variable", {
  expect_true(memberships[["ECOLEX_MEM"]]$Beg[1] <
                memberships[["ECOLEX_MEM"]]$Beg[10])
  expect_true(memberships[["ECOLEX_MEM"]]$Beg[50] <
                memberships[["ECOLEX_MEM"]]$Beg[75])
  expect_true(memberships[["ECOLEX_MEM"]]$Beg[100] <
                memberships[["ECOLEX_MEM"]]$Beg[120])
})
