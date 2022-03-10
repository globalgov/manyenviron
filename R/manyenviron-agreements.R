#' agreements database documentation
#'
#' @format The agreements database is a list that contains the
#' following 5 datasets: IEADB, GNEVAR, ECOLEX, CIESIN, HEIDI.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{IEADB: }{A dataset with 3666 observations and the following
#' 10 variables: manyID, Title, Beg, L, D, Signature, Force, R, treatyID, ieadbID.}
#' \item{GNEVAR: }{A dataset with 3428 observations and the following
#' 12 variables: manyID, Title, Beg, End, L, D, J, Signature, Force, R, treatyID, gnevarID.}
#' \item{ECOLEX: }{A dataset with 2174 observations and the following
#' 10 variables: manyID, Title, Beg, L, J, Signature, Force, R, treatyID, ecolexID.}
#' \item{CIESIN: }{A dataset with 666 observations and the following
#' 7 variables: manyID, Title, Beg, Signature, Force, R, treatyID.}
#' \item{HEIDI: }{A dataset with 2280 observations and the following
#' 7 variables: manyID, Title, Beg, Signature, R, treatyID, heidiID.}
#' }

#'
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(agreements, skimr::skim_without_charts)
#' ```
"agreements"
