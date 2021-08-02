# ECOLEX References Preparation Script

# This is a template for importing, cleaning, and exporting data
# ready for the qPackage.

# Stage one: Collecting data
load("data-raw/references/REF/ecorefer.RData")

# Stage two: Correcting data
# In this stage you will want to correct the variable names and
# formats of the 'REF' object until the object created
# below (in stage three) passes all the tests.
ECOLEX <- purrr::discard(eco_refer, function(x) length(x)==1) %>%
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
