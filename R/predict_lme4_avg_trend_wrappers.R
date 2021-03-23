#' Use `predict_lmer` on groups to generate average trend and apply to original data
#'
#' `predict_lmer_avg_trend()` is a simple wrapper around [predict_lme4_avg_trend()].
#' For details surrounding the linear mixed effects model fitting, please see
#' [lme4::lmer] and for more details on the augury function this wraps around and the
#' various arguments this function accepts, please see [predict_lme4_avg_trend()].
#'
#' @inherit predict_lme4_avg_trend params return details
#'
#' @export
predict_lmer_avg_trend <- function(df,
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
                                   replace_filter = NULL,
                                   error_correct = FALSE,
                                   error_correct_cols = NULL,
                                   shift_trend = FALSE) {
  predict_lme4_avg_trend(df = df,
                         model = lme4::lmer,
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


#' Use `predict_glmer` on groups to generate average trend and apply to original data
#'
#' `predict_glmer_avg_trend()` is a simple wrapper around [predict_lme4_avg_trend()].
#' For details surrounding the generalized linear mixed effects model fitting, please see
#' [lme4::glmer] and for more details on the augury function this wraps around and the
#' various arguments this function accepts, please see [predict_lme4_avg_trend()].
#'
#' @inherit predict_lme4_avg_trend params return details
#'
#' @export
predict_glmer_avg_trend <- function(df,
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
  predict_lme4_avg_trend(df = df,
                         model = lme4::glmer,
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


#' Use `predict_nlmer` on groups to generate average trend and apply to original data
#'
#' `predict_nlmer_avg_trend()` is a simple wrapper around [predict_lme4_avg_trend()].
#' For details surrounding the generalized linear mixed effects model fitting, please see
#' [lme4::nlmer] and for more details on the augury function this wraps around and the
#' various arguments this function accepts, please see [predict_lme4_avg_trend()].
#'
#' @inherit predict_lme4_avg_trend params return details
#'
#' @export
predict_nlmer_avg_trend <- function(df,
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
  predict_lme4_avg_trend(df = df,
                         model = lme4::nlmer,
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