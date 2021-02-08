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
                       types = c("imputed", "imputed", "projected"),
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
                        types = c("imputed", "imputed", "projected"),
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
                      error_correct = error_correct,
                      error_correct_cols = error_correct_cols)
}
