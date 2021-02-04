#' Scales data in a data frame
#'
#' `scale_transform()` transform specified columns in a data frame using
#' a provided scale. If `divide = FALSE`, it's simply `x * scale`, otherwise
#' it is `x / scale`.
#'
#' @inherit probit_transform params return
#' @param scale Value to scale the vector(s) by. Defaults by 100
#' @param divide If `TRUE`, divide the vector by the scale. Otherwise, multiply.
#'
#' @export
scale_transform <- function(df,
                            cols,
                            scale = 100,
                            divide = TRUE) {
  assert_columns(df, cols)
  assert_numeric(scale, 1)
  dplyr::mutate(df, dplyr::across(dplyr::all_of(cols), scale_vec, scale = scale, divide = divide))
}

#' Scale a vector
#'
#' @param x Numeric vector.
#' @inheritParams scale_transform
scale_vec <- function(x,
                      scale,
                      divide) {
  if (divide) {
    x / scale
  } else {
    x * scale
  }
}
