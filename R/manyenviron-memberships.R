#' memberships database documentation
#'
#' @format The memberships database is a list that contains the
#' following 5 datasets: ECOLEX_MEM, GNEVAR_MEM, IEADB_MEM, TFDD_MEM, HUGGO_MEM.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{ECOLEX_MEM: }{A dataset with 25003 observations and the following
#' 10 variables: manyID, CountryID, Title, Beg, End, SignatureCountry,
#' Force, Rat, ecolexID, treatyID.}
#' \item{IEADB_MEM: }{A dataset with 15466 observations and the following
#' 12 variables: manyID, CountryID, Title, Beg, End, SignatureCountry,
#' Signature, Rat, Force, DocType, ieadbID, treatyID.}
#' \item{TFDD_MEM: }{A dataset with 2118 observations and the following
#' 8 variables: manyID, CountryID, Title, Beg, Signature, tfddID, Memberships,
#' treatyID.}
#' \item{HUGGO_MEM: }{A dataset with 35671 observations and the following
#' 13 variables: manyID, CountryID, Title, Beg, End, SignatureCountry,
#' Signature, Rat, Force, Term, Withdrawal, gnevarID, treatyID.}
#' }

#'
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(memberships, messydates::mreport)
#' ```
"memberships"
