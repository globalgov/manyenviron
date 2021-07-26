# Test if  meets the q ecosystem requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", references[["REF"]])))
  expect_false(any(grepl("^N/A$", references[["REF"]])))
  expect_false(any(grepl("^\\s$", references[["REF"]])))
  expect_false(any(grepl("^\\.$", references[["REF"]])))
  expect_false(any(grepl("N\\.A\\.$", references[["REF"]])))
  expect_false(any(grepl("n\\.a\\.$", references[["REF"]])))
})

# At least one column named ID
test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("ID", colnames(references[["REF"]]))))
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
    expect_false(any(grepl("^[:alpha:]$",
                           references[["REF"]]$Beg)))
    # expect_false(any(grepl("^[:digit:]{2}$",
    #                        references[["REF"]]$Beg)))
    # expect_false(any(grepl("^[:digit:]{3}$",
    #                        references[["REF"]]$Beg)))
    # expect_false(any(grepl("^[:digit:]{1}$",
    #                        references[["REF"]]$Beg)))
  }
})

# Dataset should be ordered according to the "Beg" column
# if the column exists
  test_that("dataset is arranged by date variable", {
    if (!is.null(references[["REF"]]$Beg)) {
  expect_true(references[["REF"]]$Beg[50] < references[["REF"]]$Beg[75])
  expect_true(references[["REF"]]$Beg[100] < references[["REF"]]$Beg[120])
    }
})
