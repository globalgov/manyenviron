#' agreements database documentation
#'
#' @format The agreements database is a list that contains the
#' following 4 datasets: IEADB, GNEVAR, ECOLEX, CIESIN.
#' For more information and references to each of the datasets used,
#' please use the `data_source()`, `data_contrast()`, and `data_evolution()`
#' functions.
#'\describe{
#' \item{IEADB: }{A dataset with 3666 observations and the following
#' 8 variables: IEADB_ID, Title, Beg, L, D, Signature, Force, qID.}
#' \item{GNEVAR: }{A dataset with 3469 observations and the following
#' 14 variables: qID, GNEVAR_ID, GvaID, L, J, D, Term, Grounds, Title,
#' Beg, Signature, Force, End, Parties.}
#' \item{ECOLEX: }{A dataset with 2174 observations and the following
#' 8 variables: ECOLEX_ID, Title, Beg, L, J, Signature, Force, qID.}
#' \item{CIESIN: }{A dataset with 666 observations and the following
#' 5 variables: Title, Beg, Signature, Force, qID.}
#' }
 "agreements"
