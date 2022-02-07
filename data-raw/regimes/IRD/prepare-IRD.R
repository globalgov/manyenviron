# IRD Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for many packages universe.

# Stage one: Collecting data
IRD <- readxl::read_excel("data-raw/regimes/IRD/IRD.xlsx")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'IRD' object until the object created
# below (in stage three) passes all the tests.
IRD <- as_tibble(IRD) %>%
  manydata::transmutate(Beg = manypkgs::standardise_dates(Formation)) %>%
  dplyr::arrange(Beg)
# manypkgs includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make IRD available
# within the package.
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence
# to certain standards.You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows)
# to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please note that the export_data() function requires a .bib file to be
# present in the data_raw folder of the package for citation purposes.
# Therefore, please make sure that you have permission to use the dataset
# that you're including in the package.
# To add a template of .bib file to package,
# run `manypkgs::add_bib(regimes, IRD)`.
manypkgs::export_data(IRD, database = "regimes",
                     URL = "https://watermark.silverchair.com/glep.2006.6.3.121.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAA0gwggNEBgkqhkiG9w0BBwagggM1MIIDMQIBADCCAyoGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQM0K6pZxCvB6N9ykiNAgEQgIIC-04EaZw11i75kMgp9nBa1O-T0ZTK3zAxW81BDKkcmkAedzoSGpLbvQCl6TeruhwJE5DnqpxV8xM2ZunFdQrvWnzWuPjQvj9wYJzud7R_Cyq_yDar5sBwX54c_tW63H_GxlNh0Otflu1u-dL2C1NdCArrVMAFUq6kI0gRCCPrbpeoo6dirCIlqvcWvoMWuCCdxqO7Fp5T8Lm1pHnuK4vTodUuSnI4PlRKnR5g8hEYftqFju6bYtD8Ei6OJnBCf5K3OQs-66PH73y7-QS7SlXUF6c-f7hSj2OswPV7Nu7CGf-jNiN03PQ8E94L728BDJZ8nnr6jgD6Qz5W_cExtQBrEDZT7d8BUP7-JecH-cR39m10OyJVwIWuvMmCM46qnUSDQ6ElKw-K5yucSXh_MWZgNAnIW-PX3kjadvq2_AcGMA4OU56k4viB9ED7vkodlfGF2jQubPVMjfzQ3JVYNR5Stsp54ETSnOZaf3M5-vj_32BzCBKIawETikx0ws3kFIeIKshaN9Q3VfavRpbOb1wPB-_jKln0Bo22Bf5a7hQC7rFRTx3IkA1s6gmr18mNQIs0guEyf9yVPFSzOAvSUdwTG4JfrkWqzhYbF5zwlhq0jRFb8P0SzvU3qM1TjssVXIgOyLI240SucXo5B6ymeMH7rsUg0stIbZrSQUor6Wplq1u6YnKt6gSDGxoRLB7PtTdQDW3jS3YnAu5kLDCodCsZgiWi_8ISU2ekBtDV7e-zex8uzPRqEbp7_CV1WwStL7oSj4PAIcooqe6q-csVV1gLEsbcWiBWQLQAw1iZIzJX95Uiv5YlN3AwlKzymwE-jaGL3Yih6z3CRA1sEtS4O6xEUC9cq8s7mL98e55HQAr4BfL6aOnoA-X-j7oV0Gnqn7Mq6Ia8cCmlCJrhflErA3yF4-GWImxYOay9AowxWLqTWLBlDl7eDULLM8Z-9fcw8zKObgutMukhDEJcOgTFuCqggWLg7D1nN271XqWiItTRdWFPPUa2Bk3vEjreTQg")
