#' Linearly interpolate data
#'
#' `linear_interpolation()` does simple linear interpolation on specified columns
#' in a data frame using [zoo::na.approx()].
#'
#' @inherit predict_simple params
#'
#' @return A data frame.
predict_simple_fn <- function(df,
                              model,
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

  if (model %in% c("both", "linear_interp")) {
    df <- dplyr::mutate(df, !!sym(pred_col) := zoo::na.approx(.data[[col]],
                                                              na.rm = FALSE))
  }

  if (model %in% c("both", "flat_extrap")) {
    df <- dplyr::mutate(df, !!sym(pred_col) := simple_extrap(.data[[col]]))
  }

  dplyr::ungroup(df)
}

#' Use linear interpolation to infill data
#'
#' `predict_simple()` does simple linear interpolation and/or flat extrapolation
#' on a column using [zoo::na.approx()]. Similar to other predict functions, it also
#' allows filling in of type and source if necessary. However, it does not provide
#' confidence bounds on the estimates, like other `predict_...` model-based
#' functions provide.
#'
#' Depending on the value of `model` passed to the function, linear interpolation,
#' flat extrapolation, or both is used on the data.
#'
#' @inherit grouped_predict_general_mdl params return
#' @param col Name of column to extrapolate/interpolate.
#' @param sort_col Name of column to arrange data frame by prior to interpolation. Defaults to `"year"`.
#' @param sort_descending Logical value on whether the sorted values should be descending. Defaults to `FALSE`.
#' @param types Types to add to missing values. The first value is for imputed
#'     values and the second is for extrapolated values.
#' @param replace_obs Character value specifying how, if at all, observations should
#'     be replaced by infilled values. By default, replaces missing values in `col`
#'     but if set to `"none"` then `col` is not changed
#'
#' @export
predict_simple <- function(df,
                           model = c("both", "flat_extrap", "linear_interp"),
                           col = "value",
                           group_col = "iso3",
                           sort_col = "year",
                           sort_descending = FALSE,
                           pred_col = "pred",
                           type_col = NULL,
                           types = c("imputed", "projected"),
                           source_col = NULL,
                           source = NULL,
                           replace_obs = c("missing", "none")) {
  # Assertions and error checking
  assert_df(df)
  model <- rlang::arg_match(model)
  assert_columns(df, col, group_col, type_col, source_col, type_col, source_col)
  assert_string_l1(pred_col)
  assert_string_l1(source)
  replace_obs <- rlang::arg_match(replace_obs)

  df <- predict_simple_fn(df = df,
                          model = model,
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
                         types = c(NA_character_, types),
                         type_group = NULL,
                         type_sort = sort_col,
                         source_col = source_col,
                         source = source,
                         replace_obs = replace_obs,
                         error_correct = FALSE,
                         error_correct_cols = NULL)

  df
}

simple_extrap <- function(x) {
  missing_x <- is.na(x)
  if (!all(missing_x)) {
    whr <- max(which(!missing_x))
    x[whr:length(x)] <- x[whr]
  }
  x
}
