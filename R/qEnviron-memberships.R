#' memberships database documentation
#'
#' @format The memberships database is a list that contains the
#' following 4 datasets: ECOLEX_MEM, GNEVAR_MEM, IEADB_MEM, TFDD_MEM.
#' For more information and references to each of the datasets used,
#' please use the `data_source()`, `data_contrast()`, and `data_evolution()`
#' functions.
#'\describe{
#' \item{ECOLEX_MEM: }{A dataset with 25003 observations and the following
#' 11 variables: qID_ref, CountryID, Title, Beg, End, SignatureC, Force, Rat, ECOLEX_ID, Memberships, qID.}
#' \item{GNEVAR_MEM: }{A dataset with 35671 observations and the following
#' 14 variables: qID_ref, CountryID, Title, Beg, End, SignatureC, Signature, Rat, Force, Term, Withdrawal, GNEVAR_ID, Memberships, qID.}
#' \item{IEADB_MEM: }{A dataset with 15466 observations and the following
#' 13 variables: qID_ref, CountryID, Title, Beg, End, SignatureC, Signature, Rat, Force, L, IEADB_ID, Memberships, qID.}
#' \item{TFDD_MEM: }{A dataset with 2118 observations and the following
#' 8 variables: qID_ref, CountryID, Title, Beg, Signature, TFDD_ID, Memberships, qID.}
#' }
 "memberships"
