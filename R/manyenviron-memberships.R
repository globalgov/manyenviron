#' memberships datacube documentation
#'
#' @format The memberships datacube is a list that contains the
#' following 4 datasets: ECOLEX_MEM, IEADB_MEM, TFDD_MEM, HUGGO_MEM.
#' For more information and references to each of the datasets used,
#' please use the `manydata::call_sources()` and `manydata::compare_dimensions()` functions.
#'\describe{
#' \item{ECOLEX_MEM: }{A dataset with 25003 observations and the following
#' 10 variables: manyID, stateID, Title, Begin, End, stateSignature, Force, Rat,
#' ecolexID, treatyID.}
#' \item{IEADB_MEM: }{A dataset with 15463 observations and the following
#' 12 variables: manyID, stateID, Title, Begin, End, stateSignature, Signature,
#' Rat, Force, DocType, ieadbID, treatyID.}
#' \item{TFDD_MEM: }{A dataset with 2118 observations and the following
#' 8 variables: manyID, stateID, Title, Begin, Signature, tfddID, Memberships,
#' treatyID.}
#' \item{HUGGO_MEM: }{A dataset with 51042 observations and the following
#' 31 variables: manyID, treatyID, Title, Begin, stateID, Signature, Force, End,
#' StateSignature, StateRatification, StateForce, StateForce2, StateForce3,
#' StateEnd, StateEnd2, gengID, ieaID, ecolexID, Accession, Succession, Term,
#' Comments, Deposit, obsolete, ProvisionalApp, Reservation, Notes,
#' stateForce_ecolex, stateForce_iea, Consent, Acceptance.}
#' }
#' @source
#' \itemize{
#' \item{ECOLEX_MEM: }{
#' J. Sommer. “Global governance in forestry: a cross-national analysis”. In: _International Journal ofSustainable Development & World Ecology_ 27.6 (2020), pp. 481-495.}
#' \item{IEADB_MEM: }{
#' R. B. Mitchell, L. B. Andonova, M. Axelrod, et al. “What we know (and could know) about internationalenvironmental agreements”. In: _Global Environmental Politics_ 20.1 (2020), pp. 103-121.}
#' \item{TFDD_MEM: }{
#' O. College of Earth and O. S. U. Atmospheric Science. _Product of the Transboundary Freshwater DisputeDatabase_. <http://transboundarywaters.science.oregonstate.edu.>. 2021.}
#' \item{HUGGO_MEM: }{
#' NA}
#' }
#' @section URL:
#' \itemize{
#' \item{ECOLEX_MEM: }{
#' \url{https://www.ecolex.org/result/?type=treaty}
#' }
#' \item{IEADB_MEM: }{
#' \url{https://iea.uoregon.edu/country-members}
#' }
#' \item{TFDD_MEM: }{
#' \url{https://transboundarywaters.science.oregonstate.edu/}
#' }
#' \item{HUGGO_MEM: }{Hand-coded data by the GGO team.}
#' }
#' @section Mapping:
#' \itemize{
#' \item{ECOLEX_MEM: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | For | Force |
#' | Rati | Rat |
#' | Sign | stateSignature |
#' | Term | End |
#' | StatID | stateID |
#' | EcolexID | ecolexID |
#' 
#' }
#' \item{IEADB_MEM: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | country | stateID |
#' | treatyname | Title |
#' | tsig | Signature |
#' | csig | stateSignature |
#' | crat | Rat |
#' | tterm | End |
#' | ceif3 | Force |
#' | ceif4 | Force2 |
#' | mitch_id | ieadbID |
#' | inclusion | DocType |
#' 
#' }
#' \item{TFDD_MEM: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | DateSigned | Signature |
#' | '2016Update ID' | tfddID |
#' | CCODE | stateID |
#' | DocumentName | Title |
#' | Signatories | Memberships |
#' 
#' }
#' }
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(memberships, messydates::mreport)
#' ```
"memberships"
