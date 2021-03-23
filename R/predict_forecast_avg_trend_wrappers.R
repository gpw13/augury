#' Use `predict_holt` on groups to generate average trend and apply to original data
#'
#' `predict_holt_avg_trend()` is a simple wrapper around [predict_forecast_avg_trend()].
#' For details surrounding the forecasting, please see
#' [forecast::holt()] and for more details on the augury function this wraps around and the
#' various arguments this function accepts, please see [predict_forecast_avg_trend()].
#'
#' @inherit predict_forecast_avg_trend params return details
#'
#' @export
predict_holt_avg_trend <- function(df,
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
                                   sort_col = "year",
                                   sort_descending = FALSE,
                                   pred_col = "pred",
                                   upper_col = "upper",
                                   lower_col = "lower",
                                   filter_na = c("predictors", "response", "all", "none"),
                                   type_col = NULL,
                                   types = c("imputed", "imputed", "projected"),
                                   source_col = NULL,
                                   source = NULL,
                                   replace_obs = c("missing", "all", "none"),
                                   replace_filter = NULL) {
  predict_forecast_avg_trend(df = df,
                             forecast_function = forecast::holt,
                             response = response,
                             average_cols = average_cols,
                             weight_col = weight_col,
                             group_models = group_models,
                             ...,
                             ret = ret,
                             scale = scale,
                             probit = probit,
                             test_col = test_col,
                             test_period = test_period,
                             test_period_flex = test_period_flex,
                             group_col = group_col,
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

#' Use `predict_ses` on groups to generate average trend and apply to original data
#'
#' `predict_ses_avg_trend()` is a simple wrapper around [predict_forecast_avg_trend()].
#' For details surrounding the forecasting, please see
#' [forecast::ses()] and for more details on the augury function this wraps around and the
#' various arguments this function accepts, please see [predict_forecast_avg_trend()].
#'
#' @inherit predict_forecast_avg_trend params return details
#'
#' @export
predict_ses_avg_trend <- function(df,
                                  response,
                                  average_cols = NULL,
                                  weight_col = NULL,
                                  group_models = FALSE,
                                  ...,
                                  ret = c("df", "all", "error", "model"),
                                  scale = NULL,
                                  probit = FALSE,
                                  test_col = NULL,
                                  test_period = NULL,
                                  test_period_flex = NULL,
                                  group_col = "iso3",
                                  sort_col = "year",
                                  sort_descending = FALSE,
                                  pred_col = "pred",
                                  upper_col = "upper",
                                  lower_col = "lower",
                                  filter_na = c("predictors", "response", "all", "none"),
                                  type_col = NULL,
                                  types = c("imputed", "imputed", "projected"),
                                  source_col = NULL,
                                  source = NULL,
                                  replace_obs = c("missing", "all", "none"),
                                  replace_filter = NULL) {
  predict_forecast_avg_trend(df = df,
                             forecast_function = forecast::ses,
                             response = response,
                             average_cols = average_cols,
                             weight_col = weight_col,
                             group_models = group_models,
                             ...,
                             ret = ret,
                             scale = scale,
                             probit = probit,
                             test_col = test_col,
                             test_period = test_period,
                             test_period_flex = test_period_flex,
                             group_col = group_col,
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
