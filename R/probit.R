#' Probit transformation for bounded data
#'
#' Similar to logit transformation but better behaved at the extremes
#'
#' @param x Numeric vector to probit transform
#' @param inverse Logical value on whether to invert the transformation
#'
#' @return A numeric vector
probit_vec <- function(x, inverse = FALSE){
  if (!inverse) {
    x[x <= 1e-8] <- 1e-8
    x[x >= 1-1e-8] <- 1-1e-8
  }
  x <- VGAM::probitlink(x, inverse = inverse)
  if (inverse) {
    x[x <= 1e-08] <- 0
    x[x >= 1-1e-8] <- 1
  }
  x
}

#' Probit transform bounded data in a data frame
#'
#' `probit_transform()` transform specified columns in a data frame using
#' [VGAM::probitlink()]. This is similar to a logit transformation but better
#' behaved at the extremes.
#'
#' @param df A data frame.
#' @param cols Names of columns to transform.
#' @param inverse Logical value indicating whether or not to inverse the transformation.
#'
#' @return A data frame.
#'
#' @export
probit_transform <- function(df,
                             cols,
                             inverse = FALSE) {
  assert_columns(df, cols)
  dplyr::mutate(df, dplyr::across(dplyr::all_of(cols), probit_vec, inverse = inverse))
}
