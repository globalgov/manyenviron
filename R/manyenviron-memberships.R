#' memberships database documentation
#'
#' @format The memberships database is a list that contains the
#' following 4 datasets: ECOLEX_MEM, IEADB_MEM, TFDD_MEM, HUGGO_MEM.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.1
#'\describe{
#' \item{ECOLEX_MEM: }{A dataset with 25003 observations and the following
#' 10 variables: manyID, CountryID, Title, Beg, End, SignatureCountry, Force,
#' Rat, ecolexID, treatyID.}
#' \item{IEADB_MEM: }{A dataset with 15466 observations and the following
#' 12 variables: manyID, CountryID, Title, Beg, End, SignatureCountry,
#' Signature, Rat, Force, DocType, ieadbID, treatyID.}
#' \item{TFDD_MEM: }{A dataset with 2118 observations and the following
#' 8 variables: manyID, CountryID, Title, Beg, Signature, tfddID, Memberships,
#' treatyID.}
#' \item{HUGGO_MEM: }{A dataset with 76954 observations and the following
#' 31 variables: manyID, treatyID, CountryID, Title, Beg, End, Signature,
#' Force, CountrySignature, CountryRat, CountryForce, CountryForce2,
#' CountryForce3, Term, CountryWithdrawal, CountryWithdrawal2, gengID,
#' ecolexID, ieaID, Comments, Deposit, obsolete, ProvisionalApp, Reservation,
#' verified, Notes, CountryForce_ecolex, CountryForce_iea, Consent, Acceptance,
#' Accession.}
#' }

#'
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(memberships, messydates::mreport)
#' ```
"memberships"
