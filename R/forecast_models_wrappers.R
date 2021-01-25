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
                         sort_col = NULL,
                         ...,
                         ret = c("df", "all", "error", "model"),
                         test_col = NULL,
                         pred_col = "pred",
                         upper_col = "upper",
                         lower_col = "lower",
                         filter_na = c("all", "response", "predictors", "none"),
                         type_col = NULL,
                         types = c("imputed", "imputed", "projected"),
                         type_group = "iso3",
                         type_sort = "year",
                         source_col = NULL,
                         source = NULL,
                         replace_obs = c("missing", "all", "none")) {
  predict_forecast(df = df,
                   forecast_function = forecast::holt,
                   response = response,
                   sort_col = sort_col,
                   ...,
                   ret = ret,
                   test_col = test_col,
                   pred_col = pred_col,
                   upper_col = upper_col,
                   lower_col = lower_col,
                   filter_na = filter_na,
                   type_col = type_col,
                   types = types,
                   type_group = type_group,
                   type_sort = type_sort,
                   source_col = source_col,
                   source = source,
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
                        sort_col = NULL,
                        ...,
                        ret = c("df", "all", "error", "model"),
                        test_col = NULL,
                        pred_col = "pred",
                        upper_col = "upper",
                        lower_col = "lower",
                        filter_na = c("all", "response", "predictors", "none"),
                        type_col = NULL,
                        types = c("imputed", "imputed", "projected"),
                        type_group = "iso3",
                        type_sort = "year",
                        source_col = NULL,
                        source = NULL,
                        replace_obs = c("missing", "all", "none")) {
  predict_forecast(df = df,
                   forecast_function = forecast::ses,
                   response = response,
                   sort_col = sort_col,
                   ...,
                   ret = ret,
                   test_col = test_col,
                   pred_col = pred_col,
                   upper_col = upper_col,
                   lower_col = lower_col,
                   filter_na = filter_na,
                   type_col = type_col,
                   types = types,
                   type_group = type_group,
                   type_sort = type_sort,
                   source_col = source_col,
                   source = source,
                   replace_obs = replace_obs)
}

#' Use Holt's linear trend exponential smoothing to forecast data by group
#'
#' `grouped_predict_holt()` is a simple wrapper that uses exponential smoothing
#' with a linear trend to forecast data by group. For details surrounding the
#' model fitting, please see [forecast::holt()] and for more details on the
#' augury function this wraps around and the various arguments this function
#' accepts, please see [predict_forecast()].
#'
#' @inherit grouped_predict_forecast params return
#'
#' @export
grouped_predict_holt <- function(df,
                                 response,
                                 group_col,
                                 sort_col = NULL,
                                 ...,
                                 ret = c("df", "all", "error", "model"),
                                 test_col = NULL,
                                 pred_col = "pred",
                                 upper_col = "upper",
                                 lower_col = "lower",
                                 filter_na = c("all", "response", "predictors", "none"),
                                 type_col = NULL,
                                 types = c("imputed", "imputed", "projected"),
                                 type_group = "iso3",
                                 type_sort = "year",
                                 source_col = NULL,
                                 source = NULL,
                                 replace_obs = c("missing", "all", "none")) {
  grouped_predict_forecast(df = df,
                           forecast_function = forecast::holt,
                           response = response,
                           group_col = group_col,
                           sort_col = sort_col,
                           ...,
                           ret = ret,
                           test_col = test_col,
                           pred_col = pred_col,
                           upper_col = upper_col,
                           lower_col = lower_col,
                           filter_na = filter_na,
                           type_col = type_col,
                           types = types,
                           type_group = type_group,
                           type_sort = type_sort,
                           source_col = source_col,
                           source = source,
                           replace_obs = replace_obs)
}

#' Use simple exponential smoothing to forecast data by group
#'
#' `grouped_predict_ses()` is a wrapper that uses simple exponential smoothing
#' to forecast data by group. For details surrounding the
#' model fitting, please see [forecast::holt()] and for more details on the
#' augury function this wraps around and the various arguments this function
#' accepts, please see [predict_forecast()].
#'
#' @inherit grouped_predict_forecast params return
#'
#' @export
grouped_predict_ses <- function(df,
                                response,
                                group_col,
                                sort_col = NULL,
                                ...,
                                ret = c("df", "all", "error", "model"),
                                test_col = NULL,
                                pred_col = "pred",
                                upper_col = "upper",
                                lower_col = "lower",
                                filter_na = c("all", "response", "predictors", "none"),
                                type_col = NULL,
                                types = c("imputed", "imputed", "projected"),
                                type_group = "iso3",
                                type_sort = "year",
                                source_col = NULL,
                                source = NULL,
                                replace_obs = c("missing", "all", "none")) {
  grouped_predict_forecast(df = df,
                           forecast_function = forecast::holt,
                           response = response,
                           group_col = group_col,
                           sort_col = sort_col,
                           ...,
                           ret = ret,
                           test_col = test_col,
                           pred_col = pred_col,
                           upper_col = upper_col,
                           lower_col = lower_col,
                           filter_na = filter_na,
                           type_col = type_col,
                           types = types,
                           type_group = type_group,
                           type_sort = type_sort,
                           source_col = source_col,
                           source = source,
                           replace_obs = replace_obs)
}
