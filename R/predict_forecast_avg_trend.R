#' Use `predict_forecast` on groups to generate average trend and apply to original data
#'
#' `predict_forecast_avg_trend()` uses time series forecasting methods to fit a model
#' to groups within the data, and then bring that fitted prediction back to the
#' original data. The function uses [forecast::forecast()], and full details and
#' explanation of arguments that it can accept is available there. The augury
#' implementation details are available at [predict_forecast()].
#' The function also allows for inputting of data type and source information
#' directly into the data frame if the `type_col` and `source_col` are specified
#' respectively.
#'
#' @inherit predict_forecast params return
#' @inherit predict_general_mdl_avg_trend params details
#'
#' @export
predict_forecast_avg_trend <- function(df,
                                       forecast_function,
                                       response,
                                       average_cols = NULL,
                                       weight_col = NULL,
                                       group_models = TRUE,
                                       ...,
                                       ret = c("df", "all", "error", "model"),
                                       scale = NULL,
                                       probit = FALSE,
                                       test_col = NULL,
                                       test_period = NULL,
                                       test_period_flex = NULL,
                                       group_col = "iso3",
                                       obs_filter = NULL,
                                       sort_col = "year",
                                       sort_descending = FALSE,
                                       pred_col = "pred",
                                       pred_upper_col = "pred_upper",
                                       pred_lower_col = "pred_lower",
                                       upper_col = "upper",
                                       lower_col = "lower",
                                       filter_na = c("predictors", "response", "all", "none"),
                                       type_col = NULL,
                                       types = c("imputed", "imputed", "projected"),
                                       source_col = NULL,
                                       source = NULL,
                                       replace_obs = c("missing", "all", "none")) {

  # Assertions and error checking
  df <- assert_df(df)
  assert_columns(df, average_cols, weight_col, response,
                 test_col, group_col, sort_col, type_col,
                 source_col)

  assert_columns_unique(response, pred_col, pred_upper_col, pred_lower_col, upper_col, lower_col, test_col, group_col, sort_col, type_col, source_col)

  if (!is.null(weight_col)) {
    assert_numeric_cols(weight_col, df)
  }

  assert_group_sort_col(response,
                        average_cols,
                        sort_col)
  assert_group_models(group_col, group_models)

  ret <- rlang::arg_match(ret)
  assert_test_col(df, test_col)
  assert_string(pred_col, 1)
  assert_string(pred_upper_col, 1)
  assert_string(pred_lower_col, 1)
  assert_string(upper_col, 1)
  assert_string(lower_col, 1)
  filter_na <- rlang::arg_match(filter_na)
  assert_string(types, 3)
  assert_string(source, 1)
  replace_obs <- rlang::arg_match(replace_obs)
  obs_filter <- parse_obs_filter(obs_filter, response)

  # Scale response variable
  if (!is.null(scale)) {
    df <- scale_transform(df, response, scale = scale)
  }

  # Transform response variable to probit space
  if (probit) {
    df <- probit_transform(df, response)
  }

  mdl_df <- fit_forecast_average_model(df = df,
                                       forecast_function = forecast_function,
                                       response = response,
                                       average_cols = average_cols,
                                       weight_col = weight_col,
                                       ...,
                                       test_col = test_col,
                                       group_col = group_col,
                                       group_models = group_models,
                                       sort_col = sort_col,
                                       sort_descending = sort_descending,
                                       pred_col = pred_col,
                                       pred_upper_col = pred_upper_col,
                                       pred_lower_col = pred_lower_col,
                                       filter_na = filter_na,
                                       ret = ret)

  mdl <- mdl_df[["mdl"]]
  avg_df <- mdl_df[["df"]]

  if (ret == "model") {
    return(mdl)
  }

  # Merge grouped predictions back to original data
  df <- merge_average_df(avg_df = avg_df,
                         df = df,
                         response = response,
                         average_cols = average_cols,
                         group_col = group_col,
                         obs_filter = obs_filter,
                         sort_col = sort_col,
                         pred_col = pred_col,
                         pred_upper_col = pred_upper_col,
                         pred_lower_col = pred_lower_col,
                         test_col = test_col)

  # Untransform variables
  if (probit) {
    df <- probit_transform(df,
                           c(response,
                             pred_col,
                             pred_upper_col,
                             pred_lower_col),
                           inverse = TRUE)
  }

  # Unscale variables
  if (!is.null(scale)) {
    df <- scale_transform(df,
                          c(response,
                            pred_col,
                            pred_upper_col,
                            pred_lower_col),
                          scale = scale,
                          divide = FALSE)
  }

  # Get error if being returned
  if (ret %in% c("all", "error")) {
    err <- model_error(df = df,
                       response = response,
                       test_col = test_col,
                       test_period = test_period,
                       test_period_flex = test_period_flex,
                       group_col = group_col,
                       sort_col = sort_col,
                       sort_descending = sort_descending,
                       pred_col = pred_col,
                       pred_upper_col = pred_upper_col,
                       pred_lower_col = pred_lower_col)

    if (ret == "error") {
      return(err)
    }
  }

  # Merge predictions into observations
  df <- merge_prediction(df = df,
                         response = response,
                         group_col = group_col,
                         obs_filter = obs_filter,
                         sort_col = sort_col,
                         sort_descending = sort_descending,
                         pred_col = pred_col,
                         pred_upper_col = pred_upper_col,
                         pred_lower_col = pred_lower_col,
                         upper_col = upper_col,
                         lower_col = lower_col,
                         type_col = type_col,
                         types = types,
                         source_col = source_col,
                         source = source,
                         replace_obs = replace_obs)

  if (ret == "df") {
    return(df)
  } else if (ret == "all") {
    list(df = df,
         error = err,
         model = mdl)
  }
}

#' Fit forecast model to averages and apply trend to original data
#'
#' Used within `predict_forecast_avg_trend()`, this function fits the model to the data
#' frame, working whether the model is being fit across the entire data frame or
#' being fit to each group individually. Data is filtered prior to fitting,
#' model(s) are fit, and then fitted values are generated on the original.
#'
#' If fitting models individually to each group, `mdl` will never be returned, as
#' as these are instead a large group of models. Otherwise, a list of `mdl` and `df`
#' is returned and used within `predict_forecast()`.
#'
#' @inheritParams predict_forecast_avg_trend
#' @inheritParams fit_forecast_model

#' @return List of `mdl` (fitted model) and `df` (data frame with fitted values
#'     and confidence bounds generated from the model).
fit_forecast_average_model <- function(df,
                                       forecast_function,
                                       response,
                                       average_cols,
                                       weight_col,
                                       ...,
                                       test_col,
                                       group_col,
                                       group_models,
                                       sort_col,
                                       sort_descending,
                                       pred_col,
                                       pred_upper_col,
                                       pred_lower_col,
                                       filter_na,
                                       ret) {

  # get columns that will be averaged
  cols <- get_formula_avg_cols(df,
                               response,
                               average_cols)

  # average out data frame
  grp_data <- get_average_df(df,
                             cols,
                             average_cols,
                             weight_col)

  if (!is.null(sort_col)) {
    average_cols <- average_cols[!(average_cols %in% sort_col)]
  }

  if (!group_models) {
    # Filter data for modeling
    x <- get_forecast_data(df = grp_data,
                           response = response,
                           sort_col = sort_col,
                           sort_descending = sort_descending,
                           test_col = test_col)

    # Build model
    mdl <- forecast_series(x,
                           forecast_function,
                           ...)

    if (ret == "model") {
      df <- NULL
    } else {
      # Get model predictions
      df <- predict_forecast_data(df = grp_data,
                                  forecast_obj = mdl,
                                  sort_col = sort_col,
                                  sort_descending = sort_descending,
                                  pred_col = pred_col,
                                  pred_upper_col = pred_upper_col,
                                  pred_lower_col = pred_lower_col)
    }
  } else {
    # map by group
    df_list <- dplyr::group_by(grp_data, dplyr::across(dplyr::all_of(average_cols))) %>%
      dplyr::group_split()

    df <- purrr::map_dfr(df_list, function(df) {
      x <- get_forecast_data(df = df,
                             response = response,
                             sort_col = sort_col,
                             sort_descending = sort_descending,
                             test_col = test_col)
      mdl <- forecast_series(x,
                             forecast_function,
                             ...)
      predict_forecast_data(df = df,
                            forecast_obj = mdl,
                            sort_col = sort_col,
                            sort_descending = sort_descending,
                            pred_col = pred_col,
                            pred_upper_col = pred_upper_col,
                            pred_lower_col = pred_lower_col)
    })

    mdl <- NULL
  }
  list(df = df, mdl = mdl)
}
