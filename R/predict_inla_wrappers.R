#' Use INLA for time series prediction
#'
#' `predict_inla_ts()` uses a Integrated Nested Laplace approximation to
#' fit a model and use that model to infill and project the dependent variable. See
#' [predict_inla()] for more details, but the function defaults to fitting
#' a second-order random walk with no covariates for each group individually.
#'
#' @inherit predict_inla params return
#' @param model Argument passed to [INLA::f()] to define the time series model
#'      used in the default formula. Defaults to "rw2".
#' @param family A string indicating the likelihood family. The default is
#'      `gaussian` with identity link. See `names(INLA::inla.model()$likelihood)`
#'      for a list of possible alternatives and use [INLA::inla.doc()] for
#'      detailed docs for individual families.
#' @param control.inla See [INLA::control.inla()] for details.
#'
#' @export
predict_inla_ts <- function(df,
                            model = "rw2",
                            formula = stats::as.formula(sprintf("value ~ f(year, model = '%s')", model)),
                            family = "gaussian",
                            control.predictor = list(compute = TRUE),
                            control.inla = list(strategy = "laplace"),
                            safe = FALSE,
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
                            filter_na = c("predictors", "response", "all", "none"),
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
  predict_inla(df = df,
               formula = formula,
               control.predictor = control.predictor,
               family = family,
               control.inla = control.inla,
               safe = TRUE,
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

#' Use INLA for mixed effects modeling for prediction
#'
#' `predict_inla_me()` uses a Integrated Nested Laplace approximation to
#' fit a model and use that model to infill and project the dependent variable. See
#' [predict_inla()] for more details, but the function defaults to fitting
#' a mixed-effects model with the Socio-Demographic Index and life-expectancy at
#' birth (both scaled) used as covariates and second-order random walk time
#' process incorporated.
#'
#' @inherit predict_inla params return
#' @inherit predict_inla_ts params
#' @param model Argument passed to [INLA::f()] to define the time series model
#'      used in the default formula. Defaults to "rw2".
#'
#' @export
predict_inla_me <- function(df,
                            model = "rw2",
                            formula = stats::as.formula(sprintf("value ~ sdi_scaled + e0_scaled + f(region, model = 'iid') + f(year, model = '%s')", model)),
                            control.predictor = list(compute = TRUE),
                            family = "gaussian",
                            control.inla = list(strategy = "laplace"),
                            safe = FALSE,
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
                            filter_na = c("predictors", "response", "all", "none"),
                            type_col = NULL,
                            types = c("imputed", "imputed", "projected"),
                            source_col = NULL,
                            source = NULL,
                            scenario_detail_col = NULL,
                            scenario_detail = NULL,
                            replace_obs = c("missing", "all", "none"),
                            error_correct = TRUE,
                            error_correct_cols = "iso3",
                            shift_trend = FALSE) {
  predict_inla(df = df,
               formula = formula,
               control.predictor = control.predictor,
               family = family,
               control.inla = control.inla,
               safe = FALSE,
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
