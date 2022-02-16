#' Use Bayesian analysis of additive models to infill and project data
#'
#' `predict_inla()` uses a Integrated Nested Laplace approximation to fit a model
#' and use that model to infill and project the dependent variable. The function
#' uses [INLA::inla()] to perform the model fitting and prediction, and full
#' details and explanation of arguments that it can accept is available on that page.
#' The function also allows for inputting of data type and source information
#' directly into the data frame if the `type_col` and `source_col` are specified
#' respectively.
#'
#' @inherit predict_general_mdl params return
#' @param control.predictor Used to set `compute = TRUE` to ensure that the posterior
#'     marginals of the fitted values are obtained and the mean and standard deviation
#'     of the fitted values returned for use in the infilling and predictions. Additional
#'     arguments can be passed in the `control.predictor` list, but must always include
#'     `compute = TRUE`. See [INLA::control.predictor()] for details.
#' @param ... Additional arguments passed to [INLA::inla()].
#' @param filter_na Character value specifying how, if at all, to filter `NA`
#'     values from the dataset prior to applying the model. By default, only
#'     observations with missing predictors are removed, although it can also remove
#'     rows only if they have missing dependent or independent variables, or no
#'     filtering at all. Model prediction and fitting are done in one pass with
#'     [INLA::inla()], so there will be no predictions if observations with
#'     missing dependent variables are removed.
#'
#' @export
predict_inla <- function(df,
                         formula,
                         control.predictor = list(compute = TRUE),
                         ...,
                         ret = c("df", "all", "error", "model"),
                         scale = NULL,
                         probit = FALSE,
                         test_col = NULL,
                         test_period = NULL,
                         test_period_flex = NULL,
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
                         error_correct = FALSE,
                         error_correct_cols = NULL,
                         shift_trend = FALSE) {

  # Assertions and error checking
  assert_inla()
  df <- assert_df(df)
  formula_vars <- parse_formula(formula)
  assert_columns(df, formula_vars, test_col, group_col, sort_col, type_col, source_col, error_correct_cols)
  assert_group_models(group_col, group_models)
  response <- formula_vars[1]
  assert_columns_unique(response, pred_col, pred_upper_col, pred_lower_col, lower_col, upper_col, test_col, group_col, sort_col, type_col, source_col)
  ret <- rlang::arg_match(ret)
  assert_test_col(df, test_col)
  assert_string(pred_col, 1)
  assert_string(pred_upper_col, 1)
  assert_string(pred_lower_col, 1)
  assert_string(upper_col, 1)
  assert_string(lower_col, 1)
  filter_na <- rlang::arg_match(filter_na)
  assert_string(types, 3)
  assert_string(source, 1)
  replace_obs <- rlang::arg_match(replace_obs)
  obs_filter <- parse_obs_filter(obs_filter, response)

  # Scale response variable
  if (!is.null(scale)) {
    df <- scale_transform(df, formula_vars[1], scale = scale)
  }

  # Transform response variable to probit space
  if (probit) {
    df <- probit_transform(df, formula_vars[1])
  }

  mdl_df <- fit_inla_model(df = df,
                           formula = formula,
                           control.predictor = control.predictor,
                           ...,
                           formula_vars = formula_vars,
                           test_col = test_col,
                           group_col = group_col,
                           group_models = group_models,
                           obs_filter = obs_filter,
                           sort_col = sort_col,
                           sort_descending = sort_descending,
                           pred_col = pred_col,
                           pred_upper_col = pred_upper_col,
                           pred_lower_col = pred_lower_col,
                           filter_na = filter_na,
                           ret = ret,
                           error_correct = error_correct,
                           error_correct_cols = error_correct_cols,
                           shift_trend = shift_trend)

  mdl <- mdl_df[["mdl"]]
  df <- mdl_df[["df"]]

  if (ret == "model") {
    return(mdl)
  }

  # Untransform variables
  if (probit) {
    df <- probit_transform(df,
                           c(formula_vars[1],
                             pred_col,
                             pred_upper_col,
                             pred_lower_col),
                           inverse = TRUE)
  }

  # Unscale variables
  if (!is.null(scale)) {
    df <- scale_transform(df,
                          c(formula_vars[1],
                            pred_col,
                            pred_upper_col,
                            pred_lower_col),
                          scale = scale,
                          divide = FALSE)
  }

  # Get error if being returned
  if (ret %in% c("all", "error")) {
    err <- model_error(df = df,
                       response = formula_vars[1],
                       test_col = test_col,
                       test_period = test_period,
                       test_period_flex = test_period_flex,
                       group_col = group_col,
                       sort_col = sort_col,
                       sort_descending = sort_descending,
                       pred_col = pred_col,
                       pred_upper_col = pred_upper_col,
                       pred_lower_col = pred_lower_col)

    if (ret == "error") {
      return(err)
    }
  }

  # Merge predictions into observations
  df <- merge_prediction(df = df,
                         response = formula_vars[1],
                         group_col = group_col,
                         obs_filter = obs_filter,
                         sort_col = sort_col,
                         sort_descending = sort_descending,
                         pred_col = pred_col,
                         pred_upper_col = pred_upper_col,
                         pred_lower_col = pred_lower_col,
                         upper_col = upper_col,
                         lower_col = lower_col,
                         type_col = type_col,
                         types = types,
                         source_col = source_col,
                         source = source,
                         scenario_detail_col = scenario_detail_col,
                         scenario_detail = scenario_detail,
                         replace_obs = replace_obs)

  if (ret == "df") {
    return(df)
  } else if (ret == "all") {
    list(df = df,
         error = err,
         model = mdl)
  }
}

#' Generate prediction from an INLA output object
#'
#' `predict_inla_data()` generates a prediction vector from an [INLA::inla()]
#' output object, putting this prediction back into the data frame.
#'
#'
#' @inheritParams predict_inla
#' @param model INLA model object returned by [INLA::inla()] which contains
#'     `summary.fitted.values`.
#'
#' @return A data frame.
predict_inla_data <- function(df,
                              model,
                              pred_col,
                              pred_upper_col,
                              pred_lower_col) {
  fit <- model[["summary.fitted.values"]]
  df[[pred_col]] <- fit[["mean"]]
  df[[pred_lower_col]] <- fit[["0.025quant"]]
  df[[pred_upper_col]] <- fit[["0.975quant"]]
  df
}

#' Fit INLA model to data
#'
#' Used within `predict_inla()`, this function fits the model to the data
#' frame, working whether the model is being fit across the entire data frame or
#' being fit to each group individually. Data is filtered prior to fitting,
#' model(s) are fit, and then fitted values are generated on the original.
#'
#' If fitting models individually to each group, `mdl` will never be returned, as
#' as these are instead a large group of models. Otherwise, a list of `mdl` and `df`
#' is returned and used within `predict_inla()`.
#'
#' @inheritParams predict_inla
#' @inheritParams fit_general_model

#' @return List of `mdl` (fitted model) and `df` (data frame with fitted values
#'     and confidence bounds generated from the model).
fit_inla_model <- function(df,
                           formula,
                           control.predictor,
                           ...,
                           formula_vars,
                           test_col,
                           group_col,
                           group_models,
                           obs_filter,
                           sort_col,
                           sort_descending,
                           pred_col,
                           pred_upper_col,
                           pred_lower_col,
                           filter_na,
                           ret,
                           error_correct,
                           error_correct_cols,
                           shift_trend) {
  # Filter data for modeling
  if (!group_models) group_col <- NULL

  df[["augury_unique_id"]] <- 1:nrow(df)

  data <- get_model_data(df = df,
                         formula_vars = formula_vars,
                         test_col = test_col,
                         group_col = group_col,
                         filter_na = filter_na,
                         reduce_columns = FALSE)

  if (group_models) {

    # Split data frames
    data <- dplyr::group_by(data, .data[[group_col]]) %>%
      dplyr::group_split()

    # Map modeling behavior
    data <- purrr::map_dfr(data,
                           function(x) {
                             obs_check <- dplyr::filter(x, eval(parse(text = obs_filter)))
                             if (nrow(obs_check) == 0) {
                               mdl <- INLA::inla(formula = formula,
                                                 data = x,
                                                 control.predictor = control.predictor,
                                                 ...)
                               predict_inla_data(x,
                                                 mdl,
                                                 pred_col,
                                                 pred_upper_col,
                                                 pred_lower_col)
                             } else {
                               x
                             }
                           })
    data <- augury_add_columns(data, c(pred_col, pred_upper_col, pred_lower_col))

    mdl <- NULL # not returning all models together for grouped models
  } else { # single model fitting

    mdl <- INLA::inla(formula = formula,
                      data = data,
                      control.predictor = control.predictor,
                      ...)

    # don't predict data if only returning model
    if (ret == "mdl") {
      data <- NULL
    } else {
      data <- predict_inla_data(data,
                                mdl,
                                pred_col,
                                pred_upper_col,
                                pred_lower_col)
    }

  }

  # Merge predictions data with old df
  # Ensures that response values dropped for testing are available for use
  # in error calculations

  if (is.null(data)) {
    df <- NULL
  } else {
    df <- dplyr::left_join(dplyr::select(df, -dplyr::any_of(c(pred_col,
                                                              pred_lower_col,
                                                              pred_upper_col))),
                           dplyr::select(data, dplyr::all_of(c("augury_unique_id",
                                                               pred_col,
                                                               pred_lower_col,
                                                               pred_upper_col))),
                           by = "augury_unique_id") %>%
      dplyr::select(-"augury_unique_id")

    # Error correction if necessary
    df <- error_correct_fn(df = df,
                           response = formula_vars[1],
                           group_col = group_col,
                           sort_col = sort_col,
                           sort_descending = sort_descending,
                           pred_col = pred_col,
                           pred_upper_col = pred_upper_col,
                           pred_lower_col = pred_lower_col,
                           test_col = test_col,
                           error_correct = error_correct,
                           error_correct_cols = error_correct_cols,
                           shift_trend = shift_trend)
  }

  list(df = df, mdl = mdl)
}
