#' Use Holt's linear trend exponential smoothing to forecast data
#'
#' `predict_holt()` is a simple wrapper that uses exponential smoothing with
#' a linear trend to forecast data. For details surrounding the model fitting,
#' please see [forecast::holt()] and for more details on the augury function
#' this wraps around and the various arguments this function accepts, please
#' see [predict_forecast()].
#'
#' @inherit predict_forecast params return
#'
#' @export
predict_holt <- function(df,
                         response,
                         ...,
                         ret = c("df", "all", "error", "model"),
                         scale = NULL,
                         probit = FALSE,
                         test_col = NULL,
                         group_col = "iso3",
                         group_models = TRUE,
                         obs_filter = NULL,
                         sort_col = "year",
                         sort_descending = FALSE,
                         pred_col = "pred",
                         pred_upper_col = "pred_upper",
                         pred_lower_col = "pred_lower",
                         upper_col = "upper",
                         lower_col = "lower",
                         filter_na = c("all", "response", "predictors", "none"),
                         type_col = NULL,
                         types = "projected",
                         source_col = NULL,
                         source = NULL,
                         scenario_detail_col = NULL,
                         scenario_detail = NULL,
                         replace_obs = c("missing", "all", "none")) {
  predict_forecast(df = df,
                   forecast_function = forecast::holt,
                   response = response,
                   ...,
                   ret = ret,
                   scale = scale,
                   probit = probit,
                   test_col = test_col,
                   group_col = group_col,
                   group_models = group_models,
                   obs_filter = obs_filter,
                   sort_col = sort_col,
                   sort_descending = sort_descending,
                   pred_col = pred_col,
                   pred_upper_col = pred_upper_col,
                   pred_lower_col = pred_lower_col,
                   upper_col = upper_col,
                   lower_col = lower_col,
                   filter_na = filter_na,
                   type_col = type_col,
                   types = types,
                   source_col = source_col,
                   source = source,
                   scenario_detail_col = scenario_detail_col,
                   scenario_detail = scenario_detail,
                   replace_obs = replace_obs)
}

#' Use simple exponential smoothing to forecast data
#'
#' `predict_ses()` is a simple wrapper that uses simple exponential smoothing to
#' forecast data. For details surrounding the model fitting, please see
#' [forecast::holt()] and for more details on the augury function this wraps
#' around and the various arguments this function accepts, please see [predict_forecast()].
#'
#' @inherit predict_forecast params return
#'
#' @export
predict_ses <- function(df,
                        response,
                        ...,
                        ret = c("df", "all", "error", "model"),
                        scale = NULL,
                        probit = FALSE,
                        test_col = NULL,
                        group_col = "iso3",
                        group_models = TRUE,
                        obs_filter = NULL,
                        sort_col = "year",
                        sort_descending = FALSE,
                        pred_col = "pred",
                        pred_upper_col = "pred_upper",
                        pred_lower_col = "pred_lower",
                        upper_col = "upper",
                        lower_col = "lower",
                        filter_na = c("all", "response", "predictors", "none"),
                        type_col = NULL,
                        types = "projected",
                        source_col = NULL,
                        source = NULL,
                        scenario_detail_col = NULL,
                        scenario_detail = NULL,
                        replace_obs = c("missing", "all", "none")) {
  predict_forecast(df = df,
                   forecast_function = forecast::ses,
                   response = response,
                   ...,
                   ret = ret,
                   scale = scale,
                   probit = probit,
                   test_col = test_col,
                   group_col = group_col,
                   group_models = group_models,
                   obs_filter = obs_filter,
                   sort_col = sort_col,
                   sort_descending = sort_descending,
                   pred_col = pred_col,
                   pred_upper_col = pred_upper_col,
                   pred_lower_col = pred_lower_col,
                   upper_col = upper_col,
                   lower_col = lower_col,
                   filter_na = filter_na,
                   type_col = type_col,
                   types = types,
                   source_col = source_col,
                   source = source,
                   scenario_detail_col = scenario_detail_col,
                   scenario_detail = scenario_detail,
                   replace_obs = replace_obs)
}
