#' memberships database documentation
#'
#' @format The memberships database is a list that contains the
#' following 4 datasets: ECOLEX_MEM, IEADB_MEM, TFDD_MEM, HUGGO_MEM.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{ECOLEX_MEM: }{A dataset with 25003 observations and the following
#' 10 variables: manyID, stateID, Title, Beg, End, stateSignature, Force, Rat,
#' ecolexID, treatyID.}
#' \item{IEADB_MEM: }{A dataset with 15466 observations and the following
#' 12 variables: manyID, stateID, Title, Beg, End, stateSignature, Signature,
#' Rat, Force, DocType, ieadbID, treatyID.}
#' \item{TFDD_MEM: }{A dataset with 2118 observations and the following
#' 8 variables: manyID, stateID, Title, Beg, Signature, tfddID, Memberships,
#' treatyID.}
#' \item{HUGGO_MEM: }{A dataset with 52462 observations and the following
#' 31 variables: manyID, treatyID, stateID, Title, Beg, End, Signature, Force,
#' stateSignature, stateRat, stateForce, stateForce2, stateForce3, Term,
#' stateWithdrawal, stateWithdrawal2, gengID, ecolexID, ieaID, Comments,
#' Deposit, obsolete, ProvisionalApp, Reservation, verified, Notes,
#' stateForce_ecolex, stateForce_iea, Consent, Acceptance, Accession.}
#' }

#'
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(memberships, messydates::mreport)
#' ```
"memberships"
