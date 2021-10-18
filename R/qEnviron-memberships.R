#' memberships database documentation
#'
#' @format The memberships database is a list that contains the
#' following 4 datasets: ECOLEX_MEM, GNEVAR_MEM, IEADB_MEM, TFDD_MEM.
#' For more information and references to each of the datasets used,
#' please use the `data_source()`, `data_contrast()`, and `data_evolution()`
#' functions.
#'\describe{
#' \item{ECOLEX_MEM: }{A dataset with 25003 observations and the following
#' 10 variables: qID_ref, CountryID, Title, Beg, End, SignatureC, Force, Rat, ECOLEX_ID, qID.}
#' \item{GNEVAR_MEM: }{A dataset with 35671 observations and the following
#' 13 variables: qID_ref, CountryID, Title, Beg, End, SignatureC, Signature, Rat, Force, Term, Withdrawal, GNEVAR_ID, qID.}
#' \item{IEADB_MEM: }{A dataset with 15466 observations and the following
#' 11 variables: qID_ref, CountryID, Title, Beg, End, SignatureC, Signature, Rat, Force, IEADB_ID, qID.}
#' \item{TFDD_MEM: }{A dataset with 2118 observations and the following
#' 7 variables: qID_ref, CountryID, Title, Beg, Signature, TFDD_ID, qID.}
#' }
 "memberships"
