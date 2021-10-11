#' memberships database documentation
#'
#' @format The memberships database is a list that contains the
#' following 4 datasets: ECOLEX_MEM, GNEVAR_MEM, IEADB_MEM, TFDD_MEM.
#' For more information and references to each of the datasets used,
#' please use the `data_source()`, `data_contrast()`, and `data_evolution()`
#' functions.
#'\describe{
#' \item{ECOLEX_MEM: }{A dataset with 25003 observations and the following
#' 10 variables: CountryID, qID_ref, Title, Beg, End, SignatureC, Rat, Force, qID, ECOLEX_ID.}
#' \item{GNEVAR_MEM: }{A dataset with 35671 observations and the following
#' 13 variables: CountryID, qID_ref, Title, Beg, End, SignatureC, Signature, Rat, Force, Term, Withdrawal, qID, GNEVAR_ID.}
#' \item{IEADB_MEM: }{A dataset with 27410 observations and the following
#' 11 variables: CountryID, qID_ref, Title, Beg, End, SignatureC, Signature, Rat, Force, qID, IEADB_ID.}
#' \item{TFDD_MEM: }{A dataset with 2118 observations and the following
#' 7 variables: CountryID, qID_ref, Title, Beg, Signature, qID, TFDD_ID.}
#' }
 "memberships"
