#' Linearly interpolate columns
#'
#' `linear_interpolation()` does simple linear interpolation on specified columns
#' in a data frame using [zoo::na.approx()].
#'
#' @inherit linear_interpolation params
#'
#' @return A data frame.
linear_interpolation_fn <- function(df,
                                    col,
                                    group_col = NULL,
                                    pred_col = "pred",
                                    sort_col = NULL,
                                    sort_descending = FALSE) {
  df <- dplyr::group_by(df, .dots = group_col)
  if (!is.null(sort_col)) {
    if (sort_descending) {
      fn <- dplyr::desc
    } else {
      fn <- NULL
    }
    df <- dplyr::arrange(df, dplyr::across(sort_col, fn))
  }
  dplyr::mutate(df, !!sym(pred_col) := zoo::na.approx(.data[[col]], na.rm = FALSE)) %>%
    dplyr::ungroup()
}

#' Use linear interpolation to infill data
#'
#' `linear_interpolation()` does simple linear interpolation on a column using
#' [zoo::na.approx()]. Similar to other predict functions, it also allows filling
#' in of type and source if necessary.
#'
#' @inherit grouped_predict_general_mdl params return
#' @param sort_col Name of column to arrange data frame by prior to interpolation. Defaults to `"year"`.
#' @param sort_descending Logical value on whether the sorted values should be descending. Defaults to `FALSE`.
#' @param type Type to add to missing values.
#' @param replace_obs Character value specifying how, if at all, observations should
#'     be replaced by infilled values. By default, replaces missing values in `col`
#'     but if set to `"none"` then `col` is not changed
#'
#' @export
linear_interpolation <- function(df,
                                 col = "value",
                                 group_col = "iso3",
                                 sort_col = "year",
                                 sort_descending = FALSE,
                                 pred_col = "pred",
                                 type_col = NULL,
                                 type = "imputed",
                                 source_col = NULL,
                                 source = NULL,
                                 replace_obs = c("missing", "none")) {
  # Assertions and error checking
  assert_df(df)
  assert_columns(df, col, group_col, type_col, source_col, type_col, source_col)
  assert_string_l1(pred_col)
  assert_string_l1(type)
  assert_string_l1(source)
  replace_obs <- rlang::arg_match(replace_obs)

  df <- linear_interpolation_fn(df = df,
                                col = col,
                                group_col = group_col,
                                pred_col = pred_col,
                                sort_col = sort_col,
                                sort_descending = sort_descending)

  # Merge predictions into observations
  df <- merge_prediction(df = df,
                         response = col,
                         pred_col = pred_col,
                         upper_col = NULL,
                         lower_col = NULL,
                         type_col = type_col,
                         types = c(NA, type, NA),
                         type_group = NULL,
                         type_sort = sort_col,
                         source_col = source_col,
                         source = source,
                         replace_obs = replace_obs,
                         error_correct = FALSE,
                         error_correct_cols = NULL)

  df
}
