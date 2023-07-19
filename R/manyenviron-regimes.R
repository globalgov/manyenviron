#' regimes database documentation
#'
#' @format The regimes database is a list that contains the
#' following 1 datasets: IRD.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{IRD: }{A dataset with 92 observations and the following
#' 7 variables: Regime, RegimeComponent, Begin, Wat, Wat2, End, RegimeElement.}
#' }
#' @source
#'\itemize{
#' \item{IRD: }{
#' [1] O. R. Young and M. Zürn. “The international regimes database: Designing and using a sophisticatedtool for institutional analysis”. In: _Global Environmental Politics_ 6.3 (2006), pp. 121-143.}
#' }
#' @section URL:
#'\itemize{
#' \item{IRD: }{ \url https://direct.mit.edu/glep/article-abstract/6/3/121/14360/The-International-Regimes-Database-Designing-and}
#' }
#' @section Mapping:
#'\itemize{
#' \item{IRD: }{
#' Variable Mapping
#'
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | Formation | Begin |
#' | Endpoint | End |
#' | Watershed | Wat |
#' | Watershed2 | Wat2 |
#' 
#' }
#' }
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(regimes, messydates::mreport)
#' ```
"regimes"
