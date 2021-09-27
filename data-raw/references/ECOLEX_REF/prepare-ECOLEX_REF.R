# ECOLEX References Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.

# Stage one: Collecting data
load("data-raw/references/ECOLEX_REF/ecorefer.RData")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'REF' object until the object created
# below (in stage three) passes all the tests.
ECOLEX_REF <- purrr::discard(eco_refer, function(x) length(x)==1) %>%
  purrr::map(function(x){
    treatyrefs <- grepl("^TRE", x)
    index <- x[1]
    if(sum(!treatyrefs)==1) paste(index, x[2], x[3:length(x)], sep = "_")
    else {
      reftypes <- which(!treatyrefs)
      k <- vector()
      for(i in reftypes){
        j <- ifelse(i == max(reftypes), 
                    length(treatyrefs), 
                    reftypes[which(reftypes==i)+1]-1)
        k <- c(k, paste(index, x[i], x[(i+1):j], sep = "_"))
      }
      k
    } 
  } ) %>% unlist() %>% stringr::str_split("_")
ECOLEX_REF <- as_tibble(t(do.call(cbind, ECOLEX_REF)))
colnames(ECOLEX_REF) <- c("Treaty1", "RefType", "Treaty2")
ECOLEX_REF

# Replace ECOLEX_ID by qID
ecoid <- qEnviron::agreements$ECOLEX
ecoid <- ecoid %>% 
  dplyr::select(ECOLEX_ID, qID)

ECOLEX_REF <- dplyr::left_join(ECOLEX_REF, ecoid, by = c("Treaty1" = "ECOLEX_ID")) %>%
  dplyr::rename(qID1 = "qID")
ECOLEX_REF <- dplyr::left_join(ECOLEX_REF, ecoid, by = c("Treaty2" = "ECOLEX_ID")) %>%
  dplyr::rename(qID2 = "qID") %>% dplyr::select(qID1, RefType, qID2)
ECOLEX_REF

# qCreate includes several functions that should help cleaning
# and standardising your data.
# Please see the vignettes or website for more details.

# Stage three: Connecting data
# Next run the following line to make ref available
# within the qPackage.
qCreate::export_data(ECOLEX_REF, database = "references", URL = "NA")
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
