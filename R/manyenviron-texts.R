#' texts database documentation
#'
#' @format The texts database is a list that contains the
#' following 1 datasets: AGR_TXT.
#' This database differs from the other ones in this package
#' as the goal is not to compare the datasets from multiple sources
#' but rather to extract the maximum number of treaty texts. 
#' For that, the first step was to create a consolidate version
#' of agreements databases to have a comprehensive list of all
#' environmental treaties. The second step was to extract treaty
#' texts from IEADB website by scrapping the webpages where the
#' treaty texts were available. The observations left in this dataset
#' have been completed with ECOLEX texts by scrapping ECOLEX website.
#' The dataset will be progressively complemented when other treaty
#' texts sources will be found.
#' The aim is to obtain a comprehensive list of environmental treaties
#' with their corresponding texts, if possible in English.
#'\describe{
#' \item{AGR_TXT: }{A dataset with 6377 observations and the following
#' 8 variables: qID_ref, Title, Beg, Text, Source, IEADB_ID, GNEVAR_ID, ECOLEX_ID.}
#' }
 "texts"
