#' agreements database documentation
#'
#' @format The agreements database is a list that contains the
#' following 4 datasets: IEADB, GNEVAR, ECOLEX, CIESIN.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{IEADB: }{A dataset with 3666 observations and the following
#' 9 variables: qID_ref, Title, Beg, L, D, Signature, Force, qID, IEADB_ID.}
#' \item{GNEVAR: }{A dataset with 3450 observations and the following
#' 11 variables: qID_ref, Title, Beg, End, L, D, J, Signature, Force, qID, GNEVAR_ID.}
#' \item{ECOLEX: }{A dataset with 2174 observations and the following
#' 9 variables: qID_ref, Title, Beg, L, J, Signature, Force, qID, ECOLEX_ID.}
#' \item{CIESIN: }{A dataset with 666 observations and the following
#' 6 variables: qID_ref, Title, Beg, Signature, Force, qID.}
#' }
 "agreements"
