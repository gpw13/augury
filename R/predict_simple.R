#' Linearly interpolate data
#'
#' `predict_simple_fn()` does simple linear interpolation and flat extrapolation
#' on specified columnn in a data frame using [zoo::na.approx()].
#'
#' @inherit predict_simple params
#'
#' @return A data frame.
predict_simple_fn <- function(df,
                              model,
                              col,
                              test_col = NULL,
                              group_col = NULL,
                              obs_filter = NULL,
                              pred_col = "pred",
                              sort_col = NULL,
                              sort_descending = FALSE) {
  df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group_col)))

  if (!is.null(sort_col)) {
    if (sort_descending) {
      fn <- dplyr::desc
    } else {
      fn <- NULL
    }
    df <- dplyr::arrange(df, dplyr::across(sort_col, fn), .by_group = TRUE)
  }

  df <- dplyr::mutate(df,
                      !!sym(pred_col) := if (!is.null(test_col)) ifelse(.data[[test_col]], NA_real_, .data[[col]]) else .data[[col]],
                      !!sym(pred_col) := dplyr::case_when(eval(parse(text = obs_filter)) ~ NA_real_,
                                                          TRUE ~ .data[[pred_col]]))

  if (model %in% c("forward", "all", "linear_interp")) {
    df <- dplyr::mutate(df, !!sym(pred_col) := zoo::na.approx(.data[[pred_col]],
                                                              na.rm = FALSE))
  }

  if (model %in% c("all", "flat_extrap", "back_extrap", "both_extrap", "forward")) {
    df <- dplyr::mutate(df, !!sym(pred_col) := simple_extrap(.data[[pred_col]], model = model))
  }

  dplyr::ungroup(df)
}

#' Use linear interpolation and flat extrapolation to infill data
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
#' @inherit predict_general_mdl params return
#'
#' @param model Type of simple extrapolation or interpolation to perform:
#' \itemize{
#'   \item{`forward`: }{Just `flat_extrap` and `linear_interp`. (default)}
#'   \item{`all`: }{All of `flat_extrap`, `linear_interp`, and `back_extrap`}
#'   \item{`flat_extrap`: }{Flat extrapolation from latest observed point.}
#'   \item{`linear_interp`: }{Linear interpolation between observed data points.}
#'   \item{`back_extrap`: }{Flat extrapolation from first observed data point backwards.}
#'   \item{`both_extrap`: }{Both `flat_extrap` and `back_extrap`.}
#' }
#' @param col Name of column to extrapolate/interpolate.
#' @param types Types to add to missing values. The first value is for imputed
#'     values and the second is for extrapolated values.
#' @param replace_obs Character value specifying how, if at all, observations should
#'     be replaced by infilled values. By default, replaces missing values in `col`
#'     but if set to `"none"` then `col` is not changed.
#'
#' @export
predict_simple <- function(df,
                           model = c("forward", "all", "flat_extrap", "linear_interp", "back_extrap", "both_extrap"),
                           col = "value",
                           ret = c("df", "all", "error"),
                           test_col = NULL,
                           test_period = NULL,
                           test_period_flex = NULL,
                           group_col = "iso3",
                           obs_filter = NULL,
                           sort_col = "year",
                           sort_descending = FALSE,
                           pred_col = "pred",
                           type_col = NULL,
                           types = c("imputed", "imputed", "projected"),
                           source_col = NULL,
                           source = NULL,
                           replace_obs = c("missing", "none")) {
  # Assertions and error checking
  df <- assert_df(df)
  model <- rlang::arg_match(model)
  ret <- rlang::arg_match(ret)
  assert_columns(df, col, group_col, test_col, type_col, source_col, type_col, source_col)
  assert_columns_unique(col, group_col, pred_col, test_col, type_col, source_col)
  assert_string(pred_col, 1)
  assert_string(source, 1)
  assert_string(types, 3)
  replace_obs <- rlang::arg_match(replace_obs)
  obs_filter <- parse_obs_filter(obs_filter, col)

  df <- predict_simple_fn(df = df,
                          model = model,
                          col = col,
                          test_col = test_col,
                          group_col = group_col,
                          obs_filter = obs_filter,
                          pred_col = pred_col,
                          sort_col = sort_col,
                          sort_descending = sort_descending)

  # Calculate error if necessary
  if (ret %in% c("all", "error")) {
    err <- model_error(df = df,
                       response = col,
                       test_col = test_col,
                       test_period = test_period,
                       test_period_flex = test_period_flex,
                       group_col = group_col,
                       sort_col = sort_col,
                       sort_descending = sort_descending,
                       pred_col = pred_col,
                       upper_col = NULL,
                       lower_col = NULL)

    if (ret == "error") {
      return(err)
    }
  }

  # Merge predictions into observations
  df <- merge_prediction(df = df,
                         response = col,
                         group_col = group_col,
                         obs_filter = obs_filter,
                         sort_col = sort_col,
                         sort_descending = sort_descending,
                         pred_col = pred_col,
                         type_col = type_col,
                         types = types,
                         source_col = source_col,
                         source = source,
                         replace_obs = replace_obs)

  # Return what we need
  if (ret == "df") {
    return(df)
  } else if (ret == "all") {
    list(df = df,
         error = err)
  }
}

#' Helper function to do flat extrapolation
#'
#' @param x Vector to do flat extrapolation on
#' @param model Type of extrapolation to do (backward or forward)
simple_extrap <- function(x, model) {
  missing_x <- is.na(x)
  if (!all(missing_x)) {
    if (model %in% c("all", "flat_extrap", "both_extrap", "forward")) {
      whr <- max(which(!missing_x))
      x[whr:length(x)] <- x[whr]
    }
    if (model %in% c("all", "both_extrap", "back_extrap")) {
      whr <- min(which(!missing_x))
      x[1:whr] <- x[whr]
    }
  }
  x
}
