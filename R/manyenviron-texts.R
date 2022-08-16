#' texts database documentation
#'
#' @import manydata
#' @format The texts database is a list that contains the
#' following 1 datasets: GNEVAR_TXT.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{GNEVAR_TXT: }{A dataset with 6377 observations and the following
#' 8 variables: manyID, Title, Beg, TreatyText, Source, ieadbID, gnevarID,
#' ecolexID.}
#' }
#' 
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(texts, messydates::mreport)
#' ```
"texts"
