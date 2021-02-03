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
                            formula = stats::as.formula(sprintf("value ~ f(year_n, model = '%s')", model)),
                            family = "gaussian",
                            control.predictor = list(compute = TRUE),
                            control.inla = list(strategy = "laplace"),
                            ...,
                            ret = c("df", "all", "error", "model"),
                            test_col = NULL,
                            group_col = "iso3",
                            group_models = TRUE,
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
                            error_correct = FALSE,
                            error_correct_cols = NULL) {
  predict_inla(df = df,
               formula = formula,
               control.predictor = control.predictor,
               family = family,
               control.inla = control.inla,
               ...,
               ret = ret,
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
                            formula = stats::as.formula(sprintf("value ~ sdi_scaled + e0_scaled + f(region, model = 'iid') + f(year_n, model = '%s')", model)),
                            control.predictor = list(compute = TRUE),
                            family = "gaussian",
                            control.inla = list(strategy = "laplace"),
                            ...,
                            ret = c("df", "all", "error", "model"),
                            test_col = NULL,
                            group_col = "iso3",
                            group_models = FALSE,
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
                            error_correct = TRUE,
                            error_correct_cols = "iso3") {
  predict_inla(df = df,
               formula = formula,
               control.predictor = control.predictor,
               family = family,
               control.inla = control.inla,
               ...,
               ret = ret,
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
