# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", organizations[["MIA"]])))
  expect_false(any(grepl("^N/A$", organizations[["MIA"]])))
  expect_false(any(grepl("^\\s$", organizations[["MIA"]])))
  expect_false(any(grepl("^\\.$", organizations[["MIA"]])))
  expect_false(any(grepl("N\\.A\\.$", organizations[["MIA"]])))
  expect_false(any(grepl("n\\.a\\.$", organizations[["MIA"]])))
})

# Labels are standardized
test_that("labels are standardised", {
  if (!is.null(organizations[["MIA"]]$Label)) {
  expect_false(any(grepl("U.S.", organizations[["MIA"]])))
  expect_false(any(grepl("U.K.", organizations[["MIA"]])))
  expect_false(any(grepl("!", organizations[["MIA"]])))
  expect_false(any(grepl("NANA.", organizations[["MIA"]])))
  }
})

# Date columns should be in messydt class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(lubridate::is.Date(organizations[["MIA"]]))
  expect_false(lubridate::is.POSIXct(organizations[["MIA"]]))
  expect_false(lubridate::is.POSIXlt(organizations[["MIA"]]))
})

# Dates are standardized
test_that("Columns with dates are standardized", {
  if (!is.null(organizations[["MIA"]]$Beg)) {
    expect_equal(class(organizations[["MIA"]]$Beg), "messydt")
    expect_false(any(grepl("/", organizations[["MIA"]]$Beg)))
    expect_false(any(grepl("^[:alpha:]$",
                           organizations[["MIA"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{2}$",
                           organizations[["MIA"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{3}$",
                           organizations[["MIA"]]$Beg)))
    expect_false(any(grepl("^[:digit:]{1}$",
                           organizations[["MIA"]]$Beg)))
  }
})

# Dataset should be ordered according to the "Beg" column
# if the column exists
  test_that("dataset is arranged by date variable", {
    if (!is.null(organizations[["MIA"]]$Beg)) {
  expect_true(organizations[["MIA"]]$Beg[25] < organizations[["MIA"]]$Beg[50])
    }
})
