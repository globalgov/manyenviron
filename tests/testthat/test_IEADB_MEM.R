# # Test if  meets the many packages universe requirements
# 
# # Report missing values
# test_that("missing observations are reported correctly", {
#   expect_false(any(grepl("^n/a$", memberships[["IEADB_MEM"]])))
#   expect_false(any(grepl("^N/A$", memberships[["IEADB_MEM"]])))
#   expect_false(any(grepl("^\\s$", memberships[["IEADB_MEM"]])))
#   expect_false(any(grepl("^\\.$", memberships[["IEADB_MEM"]])))
#   expect_false(any(grepl("N\\.A\\.$", memberships[["IEADB_MEM"]])))
#   expect_false(any(grepl("n\\.a\\.$", memberships[["IEADB_MEM"]])))
# })
# 
# # Uniformity tests (agreements have a source ID, a string title, a signature and
# # entry into force date)
# test_that("datasets have the required variables", {
#   expect_col_exists(memberships[["IEADB_MEM"]], c("Beg", ".*ID$"))
# })
# 
# # Date columns should be in messydt class
# test_that("Columns are not in date, POSIXct or POSIXlt class", {
#   expect_false(any(lubridate::is.Date(memberships[["IEADB_MEM"]])))
#   expect_false(any(lubridate::is.POSIXct(memberships[["IEADB_MEM"]])))
#   expect_false(any(lubridate::is.POSIXlt(memberships[["IEADB_MEM"]])))
# })
# 
# # Dates are standardized for mandatory column
# test_that("Column `Beg` has standardised dates", {
#   expect_equal(class(memberships[["IEADB_MEM"]]$Beg), "messydt")
#   expect_false(any(grepl("/", memberships[["IEADB_MEM"]]$Beg)))
#   expect_false(any(grepl("^[:alpha:]$",
#                          memberships[["IEADB_MEM"]]$Beg)))
#   expect_false(any(grepl("^[:digit:]{2}$",
#                          memberships[["IEADB_MEM"]]$Beg)))
#   expect_false(any(grepl("^[:digit:]{3}$",
#                          memberships[["IEADB_MEM"]]$Beg)))
#   expect_false(any(grepl("^[:digit:]{1}$",
#                          memberships[["IEADB_MEM"]]$Beg)))
# })
# 
# # Dataset should be ordered according to the "Beg" column
# test_that("dataset is arranged by the `Beg` variable", {
#   expect_true(memberships[["IEADB_MEM"]]$Beg[1] <
#                 memberships[["IEADB_MEM"]]$Beg[100])
#   expect_true(memberships[["IEADB_MEM"]]$Beg[120] <
#                 memberships[["IEADB_MEM"]]$Beg[220])
#   expect_true(memberships[["IEADB_MEM"]]$Beg[250] <
#                 memberships[["IEADB_MEM"]]$Beg[350])
# })
