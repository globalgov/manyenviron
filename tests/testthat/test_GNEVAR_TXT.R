# Test if the dataset meets the many packages universe requirements
# Test that certain columns exist
test_that("datasets have the required variables", {
  pointblank::expect_col_exists(texts[["GNEVAR_TXT"]],
                                pointblank::vars(Title))
  pointblank::expect_col_exists(texts[["GNEVAR_TXT"]],
                                pointblank::vars(Beg))
  expect_true(any(grepl("ID$", colnames(texts[["GNEVAR_TXT"]]))))
  pointblank::expect_col_exists(texts[["GNEVAR_TXT"]],
                                pointblank::vars(TreatyText))
})

# Date columns should be in mdate class
test_that("Columns are not in date, POSIXct or POSIXlt class", {
  expect_s3_class(texts[["GNEVAR_TXT"]]$Beg, "mdate")
})
