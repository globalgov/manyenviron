# ref Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.

# Stage one: Collecting data
REF <- load("data-raw/references/ref/ecorefer.RData")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'ref' object until the object created
# below (in stage three) passes all the tests.
REF <- as.data.frame(as.matrix(eco_refer)) %>% 
  dplyr::rename(References = V1)

REF <- as_tibble(REF)

REF$References <-gsub("c|\\(|\\)|\"", "", as.character(REF$References))
REF$References <-gsub("\\,", "", as.character(REF$References))

# Replace ECOLEX_ID by qID
ECOLEX <- qEnviron::agreements$ECOLEX
ECOLEX <- ECOLEX %>% 
  dplyr::select(ECOLEX_ID, qID)
  
ECOLEX <- as.data.frame(ECOLEX)
for (k in seq_len(nrow(ECOLEX))) {
  REF$References <- gsub(paste0(ECOLEX$ECOLEX_ID[[k]]), paste0(ECOLEX$qID[[k]]), REF$References, ignore.case = TRUE, perl = T)
}

REF$References <- stringr::str_replace_all(REF$References, "\\sby", "-by")
REF <- tidyr::separate(REF, References, into = c("Treaty1", "ReferenceType", "Treaty2", "Treaty3",
                                                 "Treaty4", "Treaty5", "Treaty6", "Treaty7",
                                                 "Treaty8", "Treaty9", "Treaty10", "Treaty11",
                                                 "Treaty12", "Treaty13", "Treaty14", "Treaty15",
                                                 "Treaty16", "Treaty17", "Treaty18", "Treaty19",
                                                 "Treaty20", "Treaty21", "Treaty22", "Treaty23",
                                                 "Treaty24", "Treaty25", "Treaty26", "Treaty27",
                                                 "Treaty28", "Treaty29", "Treaty30", "Treaty31"), sep = "\\s")

REF <- REF %>% 
  tidyr::unite(Treaty2, Treaty2:Treaty31, sep=", ")

REF$Treaty2 <- stringr::str_replace_all(REF$Treaty2, ", NA", "")
REF$ReferenceType <- stringr::str_replace_all(REF$ReferenceType, "-", " ")

REF <- REF %>% 
  dplyr::select(Treaty1, Treaty2, ReferenceType)
# qCreate includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make ref available
# within the qPackage.
qCreate::export_data(REF, database = "references", URL = "NA", package = "qEnviron")
# This function also does two additional things.
# First, it creates a set of tests for this object to ensure adherence
# to certain standards.You can hit Cmd-Shift-T (Mac) or Ctrl-Shift-T (Windows)
# to run these tests locally at any point.
# Any test failures should be pretty self-explanatory and may require
# you to return to stage two and further clean, standardise, or wrangle
# your data into the expected format.
# Second, it also creates a documentation file for you to fill in.
# Please make sure that you cite any sources appropriately and fill in as
# much detail about the variables etc as possible.
