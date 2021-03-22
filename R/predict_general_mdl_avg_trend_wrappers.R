#' Use `predict_lm` on groups to generate average trend and apply to original data
#'
#' `predict_lm_avg_trend()` is a simple wrapper around [predict_general_mdl_avg_trend()].
#' For details surrounding the linear model fitting, please see
#' [stats::lm()] and for more details on the augury function this wraps around and the
#' various arguments this function accepts, please see [predict_general_mdl_avg_trend()].
#'
#' @inherit predict_general_mdl_avg_trend params return details
#'
#' @export
predict_lm_avg_trend <- function(df,
                                 formula,
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
                                 group_col = NULL,
                                 sort_col = NULL,
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
                                 replace_filter = NULL,
                                 error_correct = FALSE,
                                 error_correct_cols = NULL,
                                 shift_trend = FALSE) {
  predict_general_mdl_avg_trend(df = df,
                                model = stats::lm,
                                formula = formula,
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
                                replace_filter = replace_filter,
                                error_correct = error_correct,
                                error_correct_cols = error_correct_cols,
                                shift_trend = shift_trend)
}

#' Use `predict_glm` on groups to generate average trend and apply to original data
#'
#' `predict_glm_avg_trend()` is a simple wrapper around [predict_general_mdl_avg_trend()].
#' For details surrounding the linear model fitting, please see
#' [stats::glm()] and for more details on the augury function this wraps around and the
#' various arguments this function accepts, please see [predict_general_mdl_avg_trend()].
#'
#' @inherit predict_general_mdl_avg_trend params return details
#'
#' @export
predict_glm_avg_trend <- function(df,
                                  formula,
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
                                  group_col = NULL,
                                  sort_col = NULL,
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
                                  replace_filter = NULL,
                                  error_correct = FALSE,
                                  error_correct_cols = NULL,
                                  shift_trend = FALSE) {
  predict_general_mdl_avg_trend(df = df,
                                model = stats::glm,
                                formula = formula,
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
                                replace_filter = replace_filter,
                                error_correct = error_correct,
                                error_correct_cols = error_correct_cols,
                                shift_trend = shift_trend)
}

