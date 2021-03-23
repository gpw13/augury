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
                         group_col = NULL,
                         group_models = FALSE,
                         sort_col = NULL,
                         sort_descending = FALSE,
                         pred_col = "pred",
                         upper_col = "upper",
                         lower_col = "lower",
                         filter_na = c("all", "response", "predictors", "none"),
                         type_col = NULL,
                         types = "projected",
                         source_col = NULL,
                         source = NULL,
                         replace_obs = c("missing", "all", "none"),
                         replace_filter = NULL) {
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
                   sort_col = sort_col,
                   sort_descending = sort_descending,
                   pred_col = pred_col,
                   upper_col = upper_col,
                   lower_col = lower_col,
                   filter_na = filter_na,
                   type_col = type_col,
                   types = types,
                   source_col = source_col,
                   source = source,
                   replace_obs = replace_obs,
                   replace_filter = replace_filter)
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
                        group_col = NULL,
                        group_models = FALSE,
                        sort_col = NULL,
                        sort_descending = FALSE,
                        pred_col = "pred",
                        upper_col = "upper",
                        lower_col = "lower",
                        filter_na = c("all", "response", "predictors", "none"),
                        type_col = NULL,
                        types = "projected",
                        source_col = NULL,
                        source = NULL,
                        replace_obs = c("missing", "all", "none"),
                        replace_filter = NULL) {
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
                   sort_col = sort_col,
                   sort_descending = sort_descending,
                   pred_col = pred_col,
                   upper_col = upper_col,
                   lower_col = lower_col,
                   filter_na = filter_na,
                   type_col = type_col,
                   types = types,
                   source_col = source_col,
                   source = source,
                   replace_obs = replace_obs,
                   replace_filter = replace_filter)
}
