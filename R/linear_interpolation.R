#' Linearly interpolate columns
#'
#' `linear_interpolation()` does simple linear interpolation on specified columns
#' in a data frame.
#'
#' @param df A data frame.
#' @param cols Names of columns to interpolate.
#' @param groups Names of column to group by when interpolating, such as country ISO3 codes.
#' @param sort_by Names of columns to arrange data frame by prior to interpolation, such as year.
#' @param sort_descending Logical value on whether the sorted values should be descending. Defaults to `FALSE`.
#'
#' @return A data frame.
linear_interpolation <- function(df,
                                 cols,
                                 groups = NULL,
                                 sort_by = NULL,
                                 sort_descending = FALSE) {
  assert_columns(df, cols, groups, sort_by)
  df <- dplyr::group_by(df, .dots = groups)
  if (!is.null(sort_by)) {
    if (sort_descending) {
      fn <- dplyr::desc
    } else {
      fn <- NULL
    }
    df <- dplyr::arrange(df, dplyr::across(sort_by, fn))
  }
  dplyr::mutate(dplyr::across(df, cols, zoo::na.approx, na.rm = FALSE)) %>%
    dplyr::ungroup()
}
