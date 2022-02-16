#' Use a linear mixed-effects model to infill and project data
#'
#' `predict_lmer()` is a simple wrapper that fits a linear mixed-effects model (LMM) to
#' infill and project data. For details surrounding the LMM fitting,
#' please see [lme4::lmer()] and for more details on the augury function
#' this wraps around and the various arguments this function accepts, please see
#' [predict_lme4()].
#'
#' @inherit predict_general_mdl params return
#'
#' @export
predict_lmer <- function(df,
                         formula,
                         ...,
                         ret = c("df", "all", "error", "model"),
                         scale = NULL,
                         probit = FALSE,
                         test_col = NULL,
                         group_col = "iso3",
                         group_models = FALSE,
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
                         types = c("imputed", "imputed", "projected"),
                         source_col = NULL,
                         source = NULL,
                         scenario_detail_col = NULL,
                         scenario_detail = NULL,
                         replace_obs = c("missing", "all", "none"),
                         error_correct = FALSE,
                         error_correct_cols = NULL,
                         shift_trend = FALSE) {
  predict_lme4(df = df,
               model = lme4::lmer,
               formula = formula,
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
               replace_obs = replace_obs,
               error_correct = error_correct,
               error_correct_cols = error_correct_cols,
               shift_trend = shift_trend)
}

#' Use a generalized linear mixed-effects model to infill and project data
#'
#' `predict_lmer()` is a simple wrapper that fits a generalized linear mixed-effects
#' model (GLMM) to infill and project data. For details surrounding the GLMM fitting,
#' please see [lme4::glmer()] and for more details on the augury function
#' this wraps around and the various arguments this function accepts, please see
#' [predict_lme4()].
#'
#' @inherit predict_general_mdl params return
#'
#' @export
predict_glmer <- function(df,
                         formula,
                         ...,
                         ret = c("df", "all", "error", "model"),
                         scale = NULL,
                         probit = FALSE,
                         test_col = NULL,
                         group_col = "iso3",
                         group_models = FALSE,
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
                         types = c("imputed", "imputed", "projected"),
                         source_col = NULL,
                         source = NULL,
                         scenario_detail_col = NULL,
                         scenario_detail = NULL,
                         replace_obs = c("missing", "all", "none"),
                         error_correct = FALSE,
                         error_correct_cols = NULL,
                         shift_trend = FALSE) {
  predict_lme4(df = df,
               model = lme4::glmer,
               formula = formula,
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
               replace_obs = replace_obs,
               error_correct = error_correct,
               error_correct_cols = error_correct_cols,
               shift_trend = shift_trend)
}

#' Use a non-linear mixed-effects model to infill and project data
#'
#' `predict_nlmer()` is a simple wrapper that fits a non-linear mixed-effects
#' model (GLMM) to infill and project data. For details surrounding the GLMM fitting,
#' please see [lme4::nlmer()] and for more details on the augury function
#' this wraps around and the various arguments this function accepts, please see
#' [predict_lme4()].
#'
#' @inherit predict_general_mdl params return
#'
#' @export
predict_nlmer <- function(df,
                          formula,
                          ...,
                          ret = c("df", "all", "error", "model"),
                          scale = NULL,
                          probit = FALSE,
                          test_col = NULL,
                          group_col = "iso3",
                          group_models = FALSE,
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
                          types = c("imputed", "imputed", "projected"),
                          source_col = NULL,
                          source = NULL,
                          scenario_detail_col = NULL,
                          scenario_detail = NULL,
                          replace_obs = c("missing", "all", "none"),
                          error_correct = FALSE,
                          error_correct_cols = NULL,
                          shift_trend = FALSE) {
  predict_lme4(df = df,
               model = lme4::nlmer,
               formula = formula,
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
               replace_obs = replace_obs,
               error_correct = error_correct,
               error_correct_cols = error_correct_cols,
               shift_trend = shift_trend)
}
