#' Use a linear model to infill and project data
#'
#' `predict_lm()` is a simple wrapper that fits a linear model to infill and project
#' data. For details surrounding the linear model fitting, please see
#' [stats::lm()] and for more details on the augury function this wraps around and the
#' various arguments this function accepts, please see [predict_general_mdl()].
#'
#' @inherit predict_general_mdl params return
#'
#' @export
predict_lm <- function(df,
                       formula,
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
                       replace_obs = c("missing", "all", "none"),
                       error_correct = FALSE,
                       error_correct_cols = NULL) {
  predict_general_mdl(df = df,
                      model = stats::lm,
                      formula = formula,
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
                      replace_obs = replace_obs,
                      error_correct = error_correct,
                      error_correct_cols = error_correct_cols)
}

#' Use a linear model to infill and project data by group
#'
#' `grouped_predict_lm()` is a simple wrapper that fits a linear model to each group
#' to infill and project data. For details surrounding the linear model fitting,
#' please see [stats::lm()] and for more details on the augury function this
#' wraps around and the various arguments this function accepts, please see
#' [grouped_predict_general_mdl()].
#'
#' @inherit grouped_predict_general_mdl params return
#'
#' @export
grouped_predict_lm <- function(df,
                               group_col = "iso3",
                               formula,
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
                               replace_obs = c("missing", "all", "none"),
                               error_correct = FALSE,
                               error_correct_cols = NULL) {
  grouped_predict_general_mdl(df = df,
                              group_col = group_col,
                              model = stats::lm,
                              formula = formula,
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
                              replace_obs = replace_obs,
                              error_correct = error_correct,
                              error_correct_cols = error_correct_cols)
}

#' Use a generalized linear model to infill and project data
#'
#' `predict_glm()` is a simple wrapper that fits a generalized linear model to
#' infill and project data. For details surrounding the generalized linear model
#' fitting, please see [stats::glm()] and for more details on the augury function
#' this wraps around and the various arguments this function accepts, please see
#' [predict_general_mdl()].
#'
#' @inherit predict_general_mdl params return
#'
#' @export
predict_glm <- function(df,
                       formula,
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
                       replace_obs = c("missing", "all", "none"),
                       error_correct = FALSE,
                       error_correct_cols = NULL) {
  predict_general_mdl(df = df,
                      model = stats::glm,
                      formula = formula,
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
                      replace_obs = replace_obs,
                      error_correct = error_correct,
                      error_correct_cols = error_correct_cols)
}

#' Use a generalized linear model to infill and project data by group
#'
#' `grouped_predict_glm()` is a simple wrapper that fits a generalized linear model
#' (GLM) to each group to infill and project data. For details surrounding the
#' GLM fitting, please see [stats::glm()] and for more details on the augury function this
#' wraps around and the various arguments this function accepts, please see
#' [grouped_predict_general_mdl()].
#'
#' @inherit grouped_predict_general_mdl params return
#'
#' @export
grouped_predict_glm <- function(df,
                                group_col = "iso3",
                                formula,
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
                                replace_obs = c("missing", "all", "none"),
                                error_correct = FALSE,
                                error_correct_cols = NULL) {
  grouped_predict_general_mdl(df = df,
                              group_col = group_col,
                              model = stats::glm,
                              formula = formula,
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
                              replace_obs = replace_obs,
                              error_correct = error_correct,
                              error_correct_cols = error_correct_cols)
}

#' Use a linear mixed-effects model to infill and project data
#'
#' `predict_lmer()` is a simple wrapper that fits a linear mixed-effects model (LMM) to
#' infill and project data. For details surrounding the LMM fitting,
#' please see [lme4::lmer()] and for more details on the augury function
#' this wraps around and the various arguments this function accepts, please see
#' [predict_general_mdl()].
#'
#' @inherit predict_general_mdl params return
#'
#' @export
predict_lmer <- function(df,
                        formula,
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
                        replace_obs = c("missing", "all", "none"),
                        error_correct = FALSE,
                        error_correct_cols = NULL) {
  predict_general_mdl(df = df,
                      model = lme4::lmer,
                      formula = formula,
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
                      replace_obs = replace_obs,
                      error_correct = error_correct,
                      error_correct_cols = error_correct_cols)
}

#' Use a linear mixed-effects model to infill and project data by group
#'
#' `grouped_predict_lmer()` is a simple wrapper that fits a linear mixed-effects model
#' (LMM) to each group to infill and project data. For details surrounding the
#' LMM fitting, please see [lme4::lmer()] and for more details on the augury function this
#' wraps around and the various arguments this function accepts, please see
#' [grouped_predict_general_mdl()].
#'
#' @inherit grouped_predict_general_mdl params return
#'
#' @export
grouped_predict_lmer <- function(df,
                                 group_col = "iso3",
                                 formula,
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
                                 replace_obs = c("missing", "all", "none"),
                                 error_correct = FALSE,
                                 error_correct_cols = NULL) {
  grouped_predict_general_mdl(df = df,
                              group_col = group_col,
                              model = lme4::lmer,
                              formula = formula,
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
                              replace_obs = replace_obs,
                              error_correct = error_correct,
                              error_correct_cols = error_correct_cols)
}
