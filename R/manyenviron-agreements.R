#' agreements datacube documentation
#'
#' @format The agreements datacube is a list that contains the
#' following 7 datasets: IEADB, ECOLEX, CIESIN, HEIDI, HUGGO, AIGGO, IRD.
#' For more information and references to each of the datasets used,
#' please use the `manydata::call_sources()` and `manydata::compare_dimensions()` functions.
#' \describe{
#' \item{IEADB: }{A dataset with 3667 observations and the following
#' 10 variables: manyID, Title, Begin, DocType, AgreementType, Signature, Force,
#' Lineage, treatyID, ieadbID.}
#' \item{ECOLEX: }{A dataset with 2174 observations and the following
#' 10 variables: manyID, Title, Begin, DocType, GeogArea, Signature, Force,
#' Lineage, treatyID, ecolexID.}
#' \item{CIESIN: }{A dataset with 666 observations and the following
#' 7 variables: manyID, Title, Begin, Signature, Force, Lineage, treatyID.}
#' \item{HEIDI: }{A dataset with 2280 observations and the following
#' 7 variables: manyID, Title, Begin, Signature, Lineage, treatyID, heidiID.}
#' \item{HUGGO: }{A dataset with 3982 observations and the following
#' 51 variables: manyID, treatyID, Title, Begin, End, Signature, Force, url,
#' AgreementType, DocType, GeogArea, gengID, ieaID, ecolexID, Parties, verified,
#' DocValidUntilDate, Notes, Download, MEA_type, Ambit, Region, subject_ecolex,
#' subject_iea, Keywords, Lineage, Sequence, AdoptedIn, Languages, Appendices,
#' Depository, DepositoryURL, Published, Website1, Website2, Secretariat,
#' SecretariatURL, UNEP, Supersedes, References, EnabledBy, AmendedBy, Lit, Data,
#' Coded, Abstract, TreatyText, Language, Orig_noneng_title, match, Coder.}
#' \item{AIGGO: }{A dataset with 3923 observations and the following
#' 13 variables: manyID, treatyID, Title, Begin, End, Signature, Force,
#' action_area, linkage, accessionC, accessionP, termination_type,
#' termination_date.}
#' \item{IRD: }{A dataset with 92 observations and the following
#' 9 variables: manyID, treatyID, Title, Begin, RegimeComponent, Wat, Wat2, End,
#' RegimeElement.}
#' }
#' @source
#' \itemize{
#' \item{IEADB: }{
#' R. B. Mitchell et al. "What we know (and could know) about international environmental agreements".
#' _Global Environmental Politics_ 20.1 (2020), pp. 103-121.
#' }
#' \item{ECOLEX: }{
#' J. Sommer. “Global governance in forestry: a cross-national analysis”.
#' _International Journal ofSustainable Development & World Ecology_ 27.6 (2020), pp. 481-495.
#' }
#' \item{CIESIN: }{
#' Centre for International Earth Science Information Network.
#' _Socioeconomic Data and Applications Center (SEDAC) Collection of Treaty Texts_ (2002).
#' }
#' \item{HEIDI: }{
#' J-F. Morin, and C. Blouin. "How environmental treaties contribute to global health governance".
#' _Globalization and health_ 15.1 (2019), pp. 1-8.
#' }
#' \item{HUGGO: }{
#' J. Hollway. Environmental agreements for manydata. 2021.
#' }
#' \item{AIGGO: }{
#' J. Hollway. Environmental agreements for manydata. 2021.
#' }
#' \item{IRD: }{
#' O. R. Young and M. Zürn. “The international regimes database: Designing and using a sophisticated tool for institutional analysis”. In: _Global Environmental Politics_ 6.3 (2006), pp. 121-143.
#' }
#' }
#' @section URL:
#' \itemize{
#' \item{IEADB: }{
#' \url{https://iea.uoregon.edu/base-agreement-list}
#' }
#' \item{ECOLEX: }{
#' \url{https://www.ecolex.org/result/?type=treaty}
#' }
#' \item{CIESIN: }{
#' \url{https://sedac.ciesin.columbia.edu/entri/}
#' }
#' \item{HEIDI: }{
#' \url{https://www.chaire-epi.ulaval.ca/en/data/heidi}
#' }
#' \item{HUGGO: }{Hand-coded data by the GGO team.}
#' \item{AIGGO: }{Machine-generated data by the GGO team.}
#' \item{IRD: }{
#' \url{https://iea.uoregon.edu/ird}
#' }
#' }
#' @section Mapping:
#' \itemize{
#' \item{IEADB: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | 'IEA# (click for add'l info)' | ieadbID |
#' | 'Treaty Name' | Title |
#' | 'Signature Date' | Signature |
#' | 'Date IEA entered into force' | Force |
#' | 'Agreement Type (level 2)' | AgreementType |
#' | Inclusion | DocType |
#' 
#' }
#' \item{ECOLEX: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | EcolexID | ecolexID |
#' | title | Title |
#' | Date | Signature |
#' | 'Entry.into.Force' | Force |
#' | Document.Type | DocType |
#' | Field.of.application | GeogArea |
#' 
#' }
#' \item{CIESIN: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | 'Treaty Title' | Title |
#' | 'Year of Agreement' | Signature |
#' | 'Year of Entry into Force' | Force |
#' 
#' }
#' \item{HEIDI: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | ID | heidiID |
#' | 'Name.of.the.agreement' | Title |
#' | signature.date | Signature |
#' 
#' }
#' \item{IRD: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | Regime | Title |
#' | Formation | Begin |
#' | Endpoint | End |
#' | Watershed | Wat |
#' | Watershed2 | Wat2 |
#' 
#' }
#' \item{HUGGO: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | NA | NA |
#' 
#' }
#' \item{AIGGO: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | NA | NA |
#' 
#' }
#' }
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(agreements, messydates::mreport)
#' ```
"agreements"
