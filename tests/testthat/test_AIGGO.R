# Test if the dataset meets the many packages universe requirements

# Report missing values
test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", agreements[["AIGGO"]])))
  expect_false(any(grepl("^N/A$", agreements[["AIGGO"]])))
  expect_false(any(grepl("^\\s$", agreements[["AIGGO"]])))
  expect_false(any(grepl("^\\.$", agreements[["AIGGO"]])))
  expect_false(any(grepl("N\\.A\\.$", agreements[["AIGGO"]])))
  expect_false(any(grepl("n\\.a\\.$", agreements[["AIGGO"]])))
})

# Uniformity tests (agreements have a source ID, a string title, a signature and
# entry into force date)
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(agreements[["AIGGO"]],
                                pointblank::vars(manyID))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_false(any(lubridate::is.Date(agreements[["AIGGO"]])))
  expect_false(any(lubridate::is.POSIXct(agreements[["AIGGO"]])))
  expect_false(any(lubridate::is.POSIXlt(agreements[["AIGGO"]])))
})
