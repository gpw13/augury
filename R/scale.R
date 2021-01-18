#' Scales data in a data frame
#'
#' `scale_transform()` transform specified columns in a data frame using
#' a provided scale. If `inverse = FALSE`, it's simply `x * scale`, otherwise
#' it is `x / scale`.
#'
#' @inherit probit_transform params return
#'
#' @export
scale_transform <- function(df,
                            cols,
                            scale = 100,
                            inverse = FALSE) {
  assert_columns(df, cols)
  dplyr::mutate(df, dplyr::across(cols), scale_vec, scale = scale, inverse = inverse)
}

#' Scale a vector
scale_vec <- function(x,
                      scale,
                      inverse) {
  if (inverse) {
    x / scale
  } else {
    x * scale
  }
}
