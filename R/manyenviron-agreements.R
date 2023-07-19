#' agreements database documentation
#'
#' @format The agreements database is a list that contains the
#' following 6 datasets: IEADB, ECOLEX, CIESIN, HEIDI, HUGGO, AIGGO.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
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
#' @source
#'\itemize{
#' \item{IEADB: }{
#' [1] R. B. Mitchell et al. "What we know (and could know) about international environmental agreements". In: _Global Environmental Politics_ 20.1 (2020), pp. 103-121.}
#' \item{ECOLEX: }{
#' [1] J. Sommer. “Global governance in forestry: a cross-national analysis”. In: _International Journal ofSustainable Development & World Ecology_ 27.6 (2020), pp. 481-495.}
#' \item{CIESIN: }{
#' [1] Centre for International Earth Science Information Network. In: _Socioeconomic Data and Applications Center (SEDAC) Collection of Treaty Texts_ (2002).}
#' \item{HEIDI: }{
#' [1] J-F. Morin, and C. Blouin. "How environmental treaties contribute to global health governance". In: _Globalization and health_ 15.1 (2019), pp. 1-8.}
#' \item{HUGGO: }{
#' [1] J. Hollway. "Environmental agreements for manydata". (2021).}
#' \item{AIGGO: }{
#' [1] J. Hollway. "Environmental agreements for manydata". (2021).}
#' }
#' @section URL:
#'\itemize{
#' \item{IEADB: }{ \url https://iea.uoregon.edu/base-agreement-list}
#' \item{ECOLEX: }{ \url https://www.ecolex.org/result/?type=treaty}
#' \item{CIESIN: }{ \url https://sedac.ciesin.columbia.edu/entri/}
#' \item{HEIDI: }{ \url https://www.chaire-epi.ulaval.ca/en/data/heidi}
#' \item{HUGGO: }{ \url https://github.com/globalgov/manyenviron}
#' \item{AIGGO: }{ \url https://github.com/globalgov/manyenviron}
#' }
#' @section Mapping:
#'\itemize{
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
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(agreements, messydates::mreport)
#' ```
"agreements"
