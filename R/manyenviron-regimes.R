#' regimes database documentation
#'
#' @format The regimes database is a list that contains the
#' following 1 datasets: IRD.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{IRD: }{A dataset with 92 observations and the following
#' 7 variables: Regime, RegimeComponent, Beg, Wat, Wat2, End, RegimeElement.}
#' }

#'
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(regimes, skimr::skim_without_charts)
#' ```
"regimes"
