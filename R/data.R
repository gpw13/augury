#' Default covariates for use in augury functions.
#'
#' A dataset containing default covariates for all WHO member states, for use
#' within augury functions. These are used if no defaults are provided
#' to the modeling functions.
#'
#' @format A data frame with `r nrow(covariates_df)` rows and `r ncol(covariates_df)` variables:
#' \describe{
#'   \item{iso3}{Country ISO3 code.}
#'   \item{year}{Year.}
#'   \item{region}{World Bank regional classification.}
#'   \item{sdi}{Socio-demographic Index, a composite calculated using publicly available on total fertility rates, mean years of schooling, and lag distributed income per capita.}
#'   \item{sdi_scaled}{Socio-demographic Index, rescaled to a 0 - 1 scale.}
#'   \item{e0}{Life expectancy at birth, total (years).}
#'   \item{e0_scaled}{Life expectancy at birth, rescaled to a 0 - 1 scale.}
#' }
#' @source \href{https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups}{Region, World Bank regional groups, 2020.}
#' @source \href{https://gpw13.github.io/#methods-for-infilling-and-projections}{Socio-demographic Index, World Health Organization calculation, 2020.}
#' @source \href{https://population.un.org/wpp/Download/Standard/Mortality/}{Life expectancy, World Population Prospects, 2019.}
"covariates_df"
