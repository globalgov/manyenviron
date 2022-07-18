#' agreements database documentation
#'
#' @format The agreements database is a list that contains the
#' following 5 datasets: IEADB, GNEVAR, ECOLEX, CIESIN, HEIDI.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{IEADB: }{A dataset with 3666 observations and the following
#' 10 variables: manyID, Title, Beg, DocType, AgreementType, Signature,
#' Force, Lineage, treatyID, ieadbID.}
#' \item{GNEVAR: }{A dataset with 3785 observations and the following
#' 14 variables: manyID, Title, Beg, End, DocType, AgreementType, GeogArea,
#' Signature, Force, Lineage, Memb.conditions, Memb.procedures, treatyID, gnevarID.}
#' \item{ECOLEX: }{A dataset with 2174 observations and the following
#' 10 variables: manyID, Title, Beg, DocType, GeogArea, Signature, Force,
#' Lineage, treatyID, ecolexID.}
#' \item{CIESIN: }{A dataset with 666 observations and the following
#' 7 variables: manyID, Title, Beg, Signature, Force, Lineage, treatyID.}
#' \item{HEIDI: }{A dataset with 2280 observations and the following
#' 7 variables: manyID, Title, Beg, Signature, Lineage, treatyID, heidiID.}
#' }
#'
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(agreements, messydates::mreport)
#' ```
"agreements"
