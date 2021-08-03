#' Use `predict_lme4` on groups to generate average trend and apply to original data
#'
#' `predict_lme4_avg_trend()` uses mixed models from lme4 to fit a model
#' to groups within the data, and then bring that fitted prediction back to the
#' original data. Full details on available mixed models can be found at
#' lme4 package page and its implementation in augury at [predict_lme4()].
#' The function also allows for inputting of data type and source information
#' directly into the data frame if the `type_col` and `source_col` are specified
#' respectively.
#'
#' @inherit predict_lme4 params return
#' @inheritParams predict_average
#'
#' @export
predict_lme4_avg_trend <- function(df,
                                   model,
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
                                   replace_obs = c("missing", "all", "none"),
                                   error_correct = FALSE,
                                   error_correct_cols = NULL,
                                   shift_trend = FALSE) {

  # Assertions and error checking
  df <- assert_df(df)
  formula_vars <- parse_formula(formula)
  assert_columns(df, average_cols, weight_col, formula_vars,
                 test_col, group_col, sort_col, type_col,
                 source_col, error_correct_cols)
  assert_group_models(group_col, group_models)

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
  assert_string(pred_upper_col, 1)
  assert_string(pred_lower_col, 1)
  assert_string(upper_col, 1)
  assert_string(lower_col, 1)
  filter_na <- rlang::arg_match(filter_na)
  assert_string(types, 3)
  assert_string(source, 1)
  replace_obs <- rlang::arg_match(replace_obs)
  obs_filter <- parse_obs_filter(obs_filter, formula_vars[1])

  # Scale response variable
  if (!is.null(scale)) {
    df <- scale_transform(df, formula_vars[1], scale = scale)
  }

  # Transform response variable to probit space
  if (probit) {
    df <- probit_transform(df, formula_vars[1])
  }

  mdl_df <- fit_lme4_average_model(df = df,
                                   model = model,
                                   formula = formula,
                                   average_cols = average_cols,
                                   weight_col = weight_col,
                                   ...,
                                   formula_vars = formula_vars,
                                   test_col = test_col,
                                   group_col = group_col,
                                   group_models = group_models,
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
                         obs_filter = obs_filter,
                         sort_col = sort_col,
                         pred_col = pred_col,
                         pred_upper_col = pred_upper_col,
                         pred_lower_col = pred_lower_col,
                         test_col = test_col)

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
                         replace_obs = replace_obs)

  if (ret == "df") {
    return(df)
  } else if (ret == "all") {
    list(df = df,
         error = err,
         model = mdl)
  }
}

#' Fit mixed model to averages and apply trend to original data
#'
#' Used within `predict_lme4_avg_trend()`, this function fits the model to the data
#' frame, working whether the model is being fit across the entire data frame or
#' being fit to each group individually. Data is filtered prior to fitting,
#' model(s) are fit, and then fitted values are generated on the original.
#'
#' If fitting models individually to each group, `mdl` will never be returned, as
#' as these are instead a large group of models. Otherwise, a list of `mdl` and `df`
#' is returned and used within `predict_lme4_mdl()`.
#'
#' @inheritParams predict_lme4_avg_trend
#' @inheritParams fit_lme4_model
#' @inheritParams fit_general_model
#'
#' @return List of `mdl` (fitted model) and `df` (data frame with fitted values
#'     and confidence bounds generated from the model).
fit_lme4_average_model <- function(df,
                                   model,
                                   formula,
                                   average_cols,
                                   weight_col,
                                   ...,
                                   formula_vars,
                                   test_col,
                                   group_col,
                                   group_models,
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
    data <- dplyr::group_by(grp_data, .data[[group_col]]) %>%
      dplyr::group_split()

    df <- dplyr::group_by(grp_data, .data[[group_col]]) %>%
      dplyr::group_split()

    # Build and apply models

    df <- purrr::map2_dfr(data, df, function(x, y) {
      mdl <- model(formula = formula,
                   data = x,
                   ...)
      predict_lme4_data(df = y,
                        model = mdl,
                        pred_col = pred_col,
                        pred_upper_col = pred_upper_col,
                        pred_lower_col = pred_lower_col)
    })

    mdl <- NULL # not returning all models together for grouped models
  } else { # single model fitting
    mdl <- model(formula = formula,
                 data = grp_data,
                 ...)

    # don't predict data if only returning model
    if (ret == "mdl") {
      df <- NULL
    } else {
      df <- predict_lme4_data(df = grp_data,
                              model = mdl,
                              pred_col = pred_col,
                              pred_upper_col = pred_upper_col,
                              pred_lower_col = pred_lower_col)
    }
  }
  if (ret != "mdl") {
    df <- error_correct_fn(df = df,
                           response = formula_vars[1],
                           group_col = average_cols,
                           sort_col = sort_col,
                           sort_descending = sort_descending,
                           pred_col = pred_col,
                           pred_upper_col = pred_upper_col,
                           pred_lower_col = pred_lower_col,
                           test_col = NULL,
                           error_correct = error_correct,
                           error_correct_cols = error_correct_cols,
                           shift_trend = shift_trend)
  }

  list(df = df, mdl = mdl)
}
