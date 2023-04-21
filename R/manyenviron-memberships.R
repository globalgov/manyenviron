#' memberships database documentation
#'
#' @format The memberships database is a list that contains the
#' following 4 datasets: ECOLEX_MEM, IEADB_MEM, TFDD_MEM, HUGGO_MEM.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{ECOLEX_MEM: }{A dataset with 24975 observations and the following
#' 10 variables: manyID, treatyID, Title, Beg, stateID, End, stateSignature, Force, Rat, ecolexID.}
#' \item{IEADB_MEM: }{A dataset with 15463 observations and the following
#' 12 variables: manyID, treatyID, Title, Beg, stateID, End, stateSignature, Signature, Rat, Force, DocType, ieadbID.}
#' \item{TFDD_MEM: }{A dataset with 1832 observations and the following
#' 8 variables: manyID, treatyID, Title, Beg, stateID, Signature, tfddID, Memberships.}
#' \item{HUGGO_MEM: }{A dataset with 52430 observations and the following
#' 31 variables: manyID, treatyID, Title, Beg, stateID, End, Signature, Force, stateSignature, stateRat, stateForce, stateForce2, stateForce3, Term, stateWithdrawal, stateWithdrawal2, gengID, ecolexID, ieaID, Comments, Deposit, obsolete, ProvisionalApp, Reservation, verified, Notes, stateForce_ecolex, stateForce_iea, Consent, Acceptance, Accession.}
#' }

#'
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(memberships, messydates::mreport)
#' ```
"memberships"
