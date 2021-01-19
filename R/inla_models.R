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
#' @param ... Additional arguments passed to [INLA::inla()].
#' @param filter_na Character value specifying how, if at all, to filter `NA`
#'     values from the dataset prior to applying the model. By default, only
#'     observations with missing predictors are removed, although it can also remove
#'     rows only if they have missing dependent or independent variables, or no
#'     filtering at all. Model prediction and fitting are done in one pass with
#'     [INLA::inla()], so there will be no predictions if observations with
#'     missing dependent variables are removed.
#'
#' @return Depending on the value passed to `ret`, either a data frame with
#'     predicted data, a vector of errors, a fitted model, or a list with all 3.
#'
#' @export
predict_inla <- function(df,
                         formula,
                         ...,
                         ret = c("df", "all", "error", "model"),
                         test_col = NULL,
                         pred_col = "pred",
                         upper_col = "upper",
                         lower_col = "lower",
                         filter_na = c("predictors", "response", "all", "none"),
                         type_col = NULL,
                         types = c("imputed", "imputed", "projected"),
                         type_group = "iso3",
                         type_sort = "year",
                         source_col = NULL,
                         source = NULL,
                         replace_obs = c("missing", "all", "none"),
                         error_correct = FALSE,
                         error_correct_cols = NULL) {
  # Assertions and error checking
  assert_df(df)
  formula_vars <- parse_formula(formula)
  assert_columns(df, formula_vars, test_col, type_col, source_col, error_correct_cols)
  ret <- rlang::arg_match(ret)
  assert_test_col(df, test_col)
  assert_string_l1(pred_col)
  assert_string_l1(upper_col)
  assert_string_l1(lower_col)
  filter_na <- rlang::arg_match(filter_na)
  assert_string_l1(type_col)
  types <- rlang::arg_match(types)
  if (!is.null(type_col)) {
    assert_columns(df, type_group, type_sort)
  }
  assert_string_l1(source)
  replace_obs <- rlang::arg_match(replace_obs)

  # Filter data for modeling
  data <- get_model_data(df = df,
                         formula_vars = formula_vars,
                         test_col = test_col,
                         filter_na = filter_na)

  # Build model
  mdl <- INLA::inla(formula = formula,
                    data = data,
                    ...)

  if (ret == "model") {
    return(mdl)
  }

  # Get model predictions
  df <- predict_inla_data(data,
                          mdl,
                          pred_col,
                          upper_col,
                          lower_col)

  # Get error if being returned
  if (ret %in% c("all", "error")) {
    err <- model_error(df,
                       formula_vars[1],
                       pred_col,
                       test_col)

    if (ret == "error") {
      return(err)
    }
  }

  # Merge predictions into observations
  df <- merge_prediction(df,
                         formula_vars[1],
                         pred_col,
                         upper_col,
                         lower_col,
                         type_col,
                         types,
                         type_group,
                         type_sort,
                         source_col,
                         source,
                         replace_obs,
                         error_correct,
                         error_correct_cols)

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
                              upper_col,
                              lower_col) {
  fit <- model[["summary.fitted.values"]]
  df[[pred_col]] <- fit[["mean"]]
  df[[upper_col]] <- fit[["0.025quant"]]
  df[[lower_col]] <- fit[["0.975quant"]]
  df
}

#' Use Bayesian analysis of additive models to infill and project data by group
#'
#' `grouped_predict_inla()` uses a Integrated Nested Laplace approximation to fit a model
#' and use that model to infill and project the dependent variable by group. It
#' uses [INLA::inla()] to perform the model fitting and prediction, and full
#' details and explanation of arguments that it can accept is available on that page.
#' The function also allows for inputting of data type and source information
#' directly into the data frame if the `type_col` and `source_col` are specified
#' respectively.
#'
#' @inherit predict_inla params
#' @inherit predict_general_mdl return
#'
#' @param group_col Column name to split data frame prior to model application.
#' @param ret Character vector specifying what values the function returns. Defaults
#'     to returning a data frame, but can return a vector of model error or a list
#'     with the data frame and error together.
#'
#' @export
grouped_predict_inla <- function(df,
                                 group_col = "iso3",
                                 formula,
                                 ...,
                                 ret = c("df", "all", "error"),
                                 test_col = NULL,
                                 pred_col = "pred",
                                 upper_col = "upper",
                                 lower_col = "lower",
                                 filter_na = c("predictors", "response", "all", "none"),
                                 type_col = NULL,
                                 types = c("imputed", "imputed", "projected"),
                                 type_group = "iso3",
                                 type_sort = "year",
                                 source_col = NULL,
                                 source = NULL,
                                 replace_obs = c("missing", "all", "none"),
                                 error_correct = FALSE,
                                 error_correct_cols = NULL) {
  # Assertions and error checking
  assert_df(df)
  formula_vars <- parse_formula(formula)
  assert_columns(df, formula_vars, test_col, type_col, source_col, error_correct_cols)
  ret <- rlang::arg_match(ret)
  assert_test_col(df, test_col)
  assert_string_l1(pred_col)
  assert_string_l1(upper_col)
  assert_string_l1(lower_col)
  filter_na <- rlang::arg_match(filter_na)
  assert_string_l1(type_col)
  types <- rlang::arg_match(types)
  if (!is.null(type_col)) {
    assert_columns(df, type_group, type_sort)
  }
  assert_string_l1(source)
  replace_obs <- rlang::arg_match(replace_obs)

  # Filter data for modeling
  data <- get_model_data(df = df,
                         formula_vars = formula_vars,
                         test_col = test_col,
                         filter_na = filter_na,
                         reduce_columns = FALSE)

  # Split data frames
  data <- dplyr::group_by(data, .data[[group_col]]) %>%
    dplyr::group_split()

  # Map modeling behavior
  df <- purrr::map_dfr(data,
                       function(x) {
                         mdl <- INLA::inla(formula = formula,
                                           data = x,
                                           ...)
                         predict_inla_data(x,
                                           mdl,
                                           pred_col,
                                           upper_col,
                                           lower_col)
                       })

  # Get error if being returned
  if (ret %in% c("all", "error")) {
    err <- model_error(df,
                       formula_vars[1],
                       pred_col,
                       test_col)

    if (ret == "error") {
      return(err)
    }
  }

  # Merge predictions into observations
  df <- merge_prediction(df,
                         formula_vars[1],
                         pred_col,
                         upper_col,
                         lower_col,
                         type_col,
                         types,
                         type_group,
                         type_sort,
                         source_col,
                         source,
                         replace_obs,
                         error_correct,
                         error_correct_cols)

  if (ret == "df") {
    return(df)
  } else if (ret == "all") {
    list(df = df,
         error = err)
  }
}
