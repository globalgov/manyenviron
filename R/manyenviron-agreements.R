#' agreements database documentation
#'
#' @format The agreements database is a list that contains the
#' following 6 datasets: IEADB, ECOLEX, CIESIN, HEIDI, HUGGO, AIGGO.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{IEADB: }{A dataset with 3666 observations and the following
#' 10 variables: manyID, Title, Begin, DocType, AgreementType, Signature, Force,
#' Lineage, treatyID, ieadbID.}
#' \item{ECOLEX: }{A dataset with 2174 observations and the following
#' 10 variables: manyID, Title, Begin, DocType, GeogArea, Signature, Force, 
#' Lineage, treatyID, ecolexID.}
#' \item{CIESIN: }{A dataset with 662 observations and the following
#' 7 variables: manyID, treatyID, Title, Begin, Signature, Force, Lineage.}
#' \item{HEIDI: }{A dataset with 2280 observations and the following
#' 7 variables: manyID, Title, Begin, Signature, Lineage, treatyID, heidiID.}
#' \item{HUGGO: }{A dataset with 3980 observations and the following
#' 50 variables: manyID, treatyID, Title, Begin, End, Signature, Force, url,
#' AgreementType, DocType, GeogArea, gengID, ieaID, ecolexID, Parties, verified,
#' DocValidUntilDate, Notes, Download, MEA_type, Ambit, Region, subject_ecolex,
#' subject_iea, Keywords, Lineage, Sequence, AdoptedIn, Languages, Appendices,
#' Depository, DepositoryURL, Published, Website1, Website2, Secretariat,
#' SecretariatURL, UNEP, Supersedes, References, EnabledBy, AmendedBy, Lit,
#' Data, Coded, Abstract, TreatyText, Language, Orig_noneng_title, match.}
#' \item{AIGGO: }{A dataset with 3923 observations and the following
#' 13 variables: manyID, treatyID, Title, Begin, End, Signature, Force,
#' action_area, linkage, accessionC, accessionP, termination_type,
#' termination_date.}
#' }

#'
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(agreements, messydates::mreport)
#' ```
"agreements"
