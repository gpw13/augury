#' Impute data using simple averages
#'
#' `predict_average_fn()` does simple imputation and flat extrapolation
#' using averages grouped by `average_cols`.
#'
#' @inherit predict_average params
#'
#' @return A data frame.
predict_average_fn <- function(df,
                               col,
                               average_cols = NULL,
                               weight_col = NULL,
                               flat_extrap = TRUE,
                               test_col = NULL,
                               group_col = NULL,
                               obs_filter = NULL,
                               pred_col = "pred",
                               sort_col = NULL,
                               sort_descending = FALSE,
                               error_correct = FALSE,
                               error_correct_cols = NULL,
                               shift_trend = FALSE) {

  # Calculate averages by groupings
  df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(average_cols)))

  # Calculate average with test set or not
  if (!is.null(test_col)) {
    if (!is.null(weight_col)) { # Calculate weighted average
      df <- df %>%
        dplyr::mutate(!!sym(pred_col) := stats::weighted.mean(.data[[col]][!.data[[test_col]]],
                                                              .data[[weight_col]][!.data[[test_col]]],
                                                              na.rm = TRUE))
    } else {
      df <- df %>%
        dplyr::mutate(!!sym(pred_col) := mean(.data[[col]][!.data[[test_col]]], na.rm = TRUE))
    }
  } else {
    if (!is.null(weight_col)) { # Calculate weighted average
      df <- df %>%
        dplyr::mutate(!!sym(pred_col) := stats::weighted.mean(.data[[col]],
                                                              .data[[weight_col]],
                                                              na.rm = TRUE))
    } else {
      df <- df %>%
        dplyr::mutate(!!sym(pred_col) := mean(.data[[col]], na.rm = TRUE))
    }
  }

  # Flat extrap from latest averages if required

  if (flat_extrap) {
    # Arranging data by other columns, `group_col` and `sort_col`
    df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group_col)))
    if (!is.null(sort_col)) {
      if (sort_descending) {
        fn <- dplyr::desc
      } else {
        fn <- I
      }
      df <- dplyr::arrange(df, dplyr::across(sort_col, fn), .by_group = TRUE)
    }

    df <- dplyr::mutate(df, !!sym(pred_col) := simple_extrap(.data[[pred_col]], "flat_extrap"))
  }

  # Error correction if applicable
  df <- error_correct_fn(df = df,
                         response = col,
                         group_col = group_col,
                         sort_col = sort_col,
                         sort_descending = sort_descending,
                         pred_col = pred_col,
                         pred_upper_col = NULL,
                         pred_lower_col = NULL,
                         test_col = test_col,
                         error_correct = error_correct,
                         error_correct_cols = error_correct_cols,
                         shift_trend = shift_trend)

  # Remove prediction column based on obs_filter
  df <- dplyr::mutate(df, !!sym(pred_col) := dplyr::case_when(
    eval(parse(text = obs_filter)) ~ NA_real_,
    TRUE ~ .data[[pred_col]])
  )

  dplyr::ungroup(df)
}

#' Use averages to impute and forecast data
#'
#' `predict_average()` does simple infilling and prediction using averages.
#' Similar to other predict functions, it also allows filling in of type and source
#' if necessary. However, it does not provide confidence bounds on the estimates,
#' like other `predict_...` model-based functions provide.
#'
#' For each year where at least 1 data point is available, the average is calculated
#' as the prediction. If `flat_extrap`, then the latest average is flat extrapolated
#' to the end of the data. When using `test_col`, the average may not be available
#' for certain groups, so flat extrapolation will be relied on, meaning that the
#' `COR` metric output by errors is difficult to interpret or use properly.
#'
#' @inheritParams predict_general_mdl
#' @inherit predict_simple params return
#' @param average_cols Column name(s) of column(s) for use in grouping data for averaging,
#'     such as regions. If missing, uses global average of the data for infilling.
#' @param weight_col Column name of column of weights to be used in averaging, such
#'     as country population.
#' @param flat_extrap Logical value determining whether or not to flat extrapolate
#'     using the latest average for missing rows with no data available.
#' @param replace_obs Character value specifying how, if at all, observations should
#'     be replaced by infilled values. By default, replaces missing values in `col`
#'     but if set to `"none"` then `col` is not changed.
#'
#' @export
predict_average <- function(df,
                            col = "value",
                            average_cols = NULL,
                            weight_col = NULL,
                            flat_extrap = TRUE,
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
                            scenario_detail_col = NULL,
                            scenario_detail = NULL,
                            replace_obs = c("missing", "all", "none"),
                            error_correct = FALSE,
                            error_correct_cols = NULL,
                            shift_trend = FALSE) {
  # Assertions and error checking
  df <- assert_df(df)
  assert_columns(df, col, average_cols, weight_col, group_col, type_col, source_col, type_col, source_col)
  assert_columns_unique(col, pred_col, group_col, type_col, source_col)
  ret <- rlang::arg_match(ret)
  assert_string(pred_col, 1)
  assert_string(types, 3)
  assert_string(source, 1)
  replace_obs <- rlang::arg_match(replace_obs)
  obs_filter <- parse_obs_filter(obs_filter, col)

  # Calculate pred column using averages
  df <- predict_average_fn(df = df,
                           col = col,
                           average_cols = average_cols,
                           weight_col = weight_col,
                           flat_extrap = flat_extrap,
                           test_col = test_col,
                           group_col = group_col,
                           obs_filter = obs_filter,
                           pred_col = pred_col,
                           sort_col = sort_col,
                           sort_descending = sort_descending,
                           error_correct = error_correct,
                           error_correct_cols = error_correct_cols,
                           shift_trend = shift_trend)

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
                       pred_upper_col = NULL,
                       pred_lower_col = NULL)

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
                         pred_upper_col = NULL,
                         pred_lower_col = NULL,
                         upper_col = NULL,
                         lower_col = NULL,
                         type_col = type_col,
                         types = types,
                         source_col = source_col,
                         source = source,
                         scenario_detail_col = scenario_detail_col,
                         scenario_detail = scenario_detail,
                         replace_obs = replace_obs)

  # Return what we need
  if (ret == "df") {
    return(df)
  } else if (ret == "all") {
    list(df = df,
         error = err)
  }
}
