#' memberships database documentation
#'
#' @format The memberships database is a list that contains the
#' following 4 datasets: ECOLEX_MEM, GNEVAR_MEM, IEADB_MEM, TFDD_MEM.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{ECOLEX_MEM: }{A dataset with 25003 observations and the following
#' 10 variables: many_ID, CountryID, Title, Beg, End, SignatureC, Force, Rat, ECOLEX_ID, treaty_ID.}
#' \item{GNEVAR_MEM: }{A dataset with 35671 observations and the following
#' 13 variables: many_ID, CountryID, Title, Beg, End, SignatureC, Signature, Rat, Force, Term, Withdrawal, GNEVAR_ID, treaty_ID.}
#' \item{IEADB_MEM: }{A dataset with 15466 observations and the following
#' 12 variables: many_ID, CountryID, Title, Beg, End, SignatureC, Signature, Rat, Force, L, IEADB_ID, treaty_ID.}
#' \item{TFDD_MEM: }{A dataset with 2118 observations and the following
#' 8 variables: many_ID, CountryID, Title, Beg, Signature, TFDD_ID, Memberships, treaty_ID.}
#' }
#' 
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(memberships, skimr::skim_without_charts)
#' ```
 "memberships"
