#' Use `predict_inla` on groups to generate average trend and apply to original data
#'
#' `predict_inla_avg_trend()` uses a Integrated Nested Laplace approximation to fit a model
#' to groups within the data, and then bring that fitted prediction back to the
#' original data. The function uses [INLA::inla()] to perform the model fitting
#' and prediction, and full details and explanation of arguments that it can accept is available on that page.
#' The function also allows for inputting of data type and source information
#' directly into the data frame if the `type_col` and `source_col` are specified
#' respectively.
#'
#' @inherit predict_inla params return
#' @inherit predict_general_mdl_avg_trend params details
#'
#' @export
predict_inla_avg_trend <- function(df,
                                   formula,
                                   average_cols = NULL,
                                   weight_col = NULL,
                                   group_models = FALSE,
                                   control.predictor = list(compute = TRUE),
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

  # Assertions and error checking
  df <- assert_df(df)
  formula_vars <- parse_formula(formula)
  assert_columns(df, average_cols, weight_col, formula_vars,
                 test_col, group_col, sort_col, type_col,
                 source_col, error_correct_cols)

  if (!is.null(weight_col)) {
    assert_numeric_cols(weight_col, df)
  }

  assert_group_sort_col(formula_vars,
                        average_cols,
                        sort_col)

  assert_error_correct_avg_trend(formula_vars,
                                 average_cols,
                                 error_correct,
                                 error_correct_cols)

  ret <- rlang::arg_match(ret)
  assert_test_col(df, test_col)
  assert_string(pred_col, 1)
  assert_string(upper_col, 1)
  assert_string(lower_col, 1)
  filter_na <- rlang::arg_match(filter_na)
  assert_string(types, 3)
  assert_string(source, 1)
  replace_obs <- rlang::arg_match(replace_obs)
  replace_filter <- parse_replace_filter(replace_filter, formula_vars[1])

  # Scale response variable
  if (!is.null(scale)) {
    df <- scale_transform(df, formula_vars[1], scale = scale)
  }

  # Transform response variable to probit space
  if (probit) {
    df <- probit_transform(df, formula_vars[1])
  }

  mdl_df <- fit_inla_average_model(df = df,
                                   formula = formula,
                                   average_cols = average_cols,
                                   weight_col = weight_col,
                                   control.predictor = control.predictor,
                                   ...,
                                   formula_vars = formula_vars,
                                   test_col = test_col,
                                   group_col = group_col,
                                   group_models = group_models,
                                   sort_col = sort_col,
                                   sort_descending = sort_descending,
                                   pred_col = pred_col,
                                   upper_col = upper_col,
                                   lower_col = lower_col,
                                   filter_na = filter_na,
                                   ret = ret,
                                   error_correct = error_correct,
                                   error_correct_cols = error_correct_cols,
                                   shift_trend = shift_trend)

  mdl <- mdl_df[["mdl"]]
  avg_df <- mdl_df[["df"]]

  if (ret == "model") {
    return(mdl)
  }

  # Merge grouped predictions back to original data
  df <- merge_average_df(avg_df = avg_df,
                         df = df,
                         response = formula_vars[1],
                         average_cols = average_cols,
                         group_col = group_col,
                         sort_col = sort_col,
                         pred_col = pred_col,
                         upper_col = upper_col,
                         lower_col = lower_col,
                         test_col = test_col)

  # Untransform variables
  if (probit) {
    df <- probit_transform(df,
                           c(formula_vars[1],
                             pred_col,
                             upper_col,
                             lower_col),
                           inverse = TRUE)
  }

  # Unscale variables
  if (!is.null(scale)) {
    df <- scale_transform(df,
                          c(formula_vars[1],
                            pred_col,
                            upper_col,
                            lower_col),
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
                       upper_col = upper_col,
                       lower_col = lower_col)

    if (ret == "error") {
      return(err)
    }
  }

  # Merge predictions into observations
  df <- merge_prediction(df = df,
                         response = formula_vars[1],
                         group_col = group_col,
                         sort_col = sort_col,
                         sort_descending = sort_descending,
                         pred_col = pred_col,
                         type_col = type_col,
                         types = types,
                         source_col = source_col,
                         source = source,
                         replace_obs = replace_obs,
                         replace_filter = replace_filter)

  if (ret == "df") {
    return(df)
  } else if (ret == "all") {
    list(df = df,
         error = err,
         model = mdl)
  }
}

#' Fit INLA model to averages and apply trend to original data
#'
#' Used within `predict_inla_avg_trend()`, this function fits the model to the data
#' frame, working whether the model is being fit across the entire data frame or
#' being fit to each group individually. Data is filtered prior to fitting,
#' model(s) are fit, and then fitted values are generated on the original.
#'
#' If fitting models individually to each group, `mdl` will never be returned, as
#' as these are instead a large group of models. Otherwise, a list of `mdl` and `df`
#' is returned and used within `predict_inla()`.
#'
#' @inheritParams predict_inla_avg_trend
#' @inheritParams fit_general_model

#' @return List of `mdl` (fitted model) and `df` (data frame with fitted values
#'     and confidence bounds generated from the model).
fit_inla_average_model <- function(df,
                                   formula,
                                   average_cols,
                                   weight_col,
                                   control.predictor,
                                   ...,
                                   formula_vars,
                                   test_col,
                                   group_col,
                                   group_models,
                                   sort_col,
                                   sort_descending,
                                   pred_col,
                                   upper_col,
                                   lower_col,
                                   filter_na,
                                   ret,
                                   error_correct,
                                   error_correct_cols,
                                   shift_trend) {

  # filter data and minimize
  data <- get_model_data(df = df,
                         formula_vars = formula_vars,
                         test_col = test_col,
                         group_col = c(average_cols, weight_col),
                         filter_na = filter_na,
                         reduce_columns = FALSE)

  # get columns that will be averaged
  cols <- get_formula_avg_cols(df,
                               formula_vars,
                               average_cols)

  # average out data frame
  grp_data <- get_average_df(df,
                             cols,
                             average_cols,
                             weight_col)

  if (!is.null(sort_col)) {
    average_cols <- average_cols[!(average_cols %in% sort_col)]
  }

  if (group_models) {

    # Split data frames

    data <- dplyr::group_by(grp_data, dplyr::across(average_cols)) %>%
      dplyr::group_split()

    # Map modeling behavior
    data <- purrr::map_dfr(data,
                           function(x) {
                             mdl <- INLA::inla(formula = formula,
                                               data = x,
                                               control.predictor = control.predictor,
                                               ...)
                             predict_inla_data(x,
                                               mdl,
                                               pred_col,
                                               upper_col,
                                               lower_col)
                           })

    mdl <- NULL # not returning all models together for grouped models
  } else { # single model fitting
    mdl <- INLA::inla(formula = formula,
                      data = grp_data,
                      control.predictor = control.predictor,
                      ...)

    # don't predict data if only returning model
    if (ret == "mdl") {
      data <- NULL
    } else {
      data <- predict_inla_data(grp_data,
                                mdl,
                                pred_col,
                                upper_col,
                                lower_col)
    }
  }
  if (ret != "mdl") {
    df <- error_correct_fn(df = data,
                           response = formula_vars[1],
                           group_col = average_cols,
                           sort_col = sort_col,
                           sort_descending = sort_descending,
                           pred_col = pred_col,
                           upper_col = upper_col,
                           lower_col = lower_col,
                           test_col = NULL,
                           error_correct = error_correct,
                           error_correct_cols = error_correct_cols,
                           shift_trend = shift_trend)
  }

  list(df = df, mdl = mdl)
}
