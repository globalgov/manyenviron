#' agreements database documentation
#'
#' @format The agreements database is a list that contains the
#' following 6 datasets: IEADB, ECOLEX, CIESIN, HEIDI, HUGGO, AIGGO.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#' @imports manydata
#'\describe{
#' \item{IEADB: }{A dataset with 3666 observations and the following
#' 10 variables: manyID, Title, Beg, DocType, AgreementType, Signature, Force,
#' Lineage, treatyID, ieadbID.}
#' \item{ECOLEX: }{A dataset with 2174 observations and the following
#' 10 variables: manyID, Title, Beg, DocType, GeogArea, Signature, Force,
#' Lineage, treatyID, ecolexID.}
#' \item{CIESIN: }{A dataset with 666 observations and the following
#' 7 variables: manyID, Title, Beg, Signature, Force, Lineage, treatyID.}
#' \item{HEIDI: }{A dataset with 2280 observations and the following
#' 7 variables: manyID, Title, Beg, Signature, Lineage, treatyID, heidiID.}
#' \item{HUGGO: }{A dataset with 3953 observations and the following
#' 51 variables: manyID, Title, Beg, End, Signature, Force, AgreementType,
#' DocType, GeogArea, gengID, ieaID, ecolexID, treatyID, Parties, verified,
#' DocValidUntilDate, url, Notes, Download, MEA_type, Ambit, Region,
#' subject_ecolex, subject_iea, Keywords, Lineage, Sequence, AdoptedIn,
#' Languages, Appendices, Depository, DepositoryURL, Published, Abstract,
#' Website1, Website2, Secretariat, SecretariatURL, UNEP, Supersedes,
#' References, EnabledBy, AmendedBy, Lit, Data, Coded, TreatyText, Source,
#' MainText, AppendixText, AnnexText.}
#' \item{AIGGO: }{A dataset with 3815 observations and the following
#' 13 variables: manyID, treatyID, Title, Beg, End, Signature, Force,
#' action_area, linkage, accessionC, accessionP, termination_type,
#' termination_date.}
#' }
#'
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(agreements, messydates::mreport)
#' ```
"agreements"
