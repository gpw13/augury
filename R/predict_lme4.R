#' Use mixed models to infill and project data
#'
#' `predict_lme4()` uses mixed models from lme4 to fit a model and
#' use that model to infill and project the dependent variable. It is flexible
#' to allow for any mixed model available in the lme4 packaged to be used through
#' the function.
#'
#' * Linear mixed models: [lme4::lmer()]
#' * Generalized linear mixed models: [lme4::glmer()]
#' * Nonlinear mixed models: [lme4::nlmer()]
#'
#' @param model An lme4 function that outputs a merMod object with that can be
#'     passed to [merTools::predictInterval()]. This should be one of [lme4::lmer()],
#'     [lme4::glmer()], or [lme4::nlmer()].
#'
#' @inherit predict_general_mdl params return
#'
#' @export
predict_lme4 <- function(df,
                         model,
                         formula,
                         ...,
                         ret = c("df", "all", "error", "model"),
                         scale = NULL,
                         probit = FALSE,
                         test_col = NULL,
                         test_period = NULL,
                         test_period_flex = NULL,
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
                         replace_filter = NULL,
                         error_correct = FALSE,
                         error_correct_cols = NULL,
                         shift_trend = FALSE) {
  # Assertions and error checking
  df <- assert_df(df)
  assert_model(model)
  formula_vars <- parse_formula(formula)
  assert_columns(df, formula_vars, test_col, group_col, sort_col, type_col, source_col)
  response <- formula_vars[1]
  assert_columns_unique(response, pred_col, lower_col, upper_col, test_col, group_col, sort_col, type_col, source_col)
  ret <- rlang::arg_match(ret)
  assert_test_col(df, test_col)
  assert_string(pred_col, 1)
  assert_string(upper_col, 1)
  assert_string(lower_col, 1)
  filter_na <- rlang::arg_match(filter_na)
  assert_string(types, 3)
  assert_string(source, 1)
  replace_obs <- rlang::arg_match(replace_obs)
  replace_filter <- parse_replace_filter(replace_filter, response)

  # Scale response variable
  if (!is.null(scale)) {
    df <- scale_transform(df, formula_vars[1], scale = scale)
  }

  # Transform response variable to probit space
  if (probit) {
    df <- probit_transform(df, formula_vars[1])
  }

  mdl_df <- fit_lme4_model(df = df,
                           model = model,
                           formula = formula,
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
  df <- mdl_df[["df"]]

  # Return model now
  if (ret == "mdl") {
    return(mdl)
  }

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

#' Generate prediction from model object
#'
#' `predict_lme4_data()` generates a prediction vector from a merMod object and full
#' data frame, putting this prediction back into the data frame.
#'
#' @inheritParams predict_lme4
#'
#' @return A data frame.
predict_lme4_data <- function(df,
                              model,
                              pred_col,
                              upper_col,
                              lower_col) {
  pred <- merTools::predictInterval(model,
                                    newdata = as.data.frame(df),
                                    level = 0.95)
  df[[pred_col]] <- pred[["fit"]]
  df[[upper_col]] <- pred[["upr"]]
  df[[lower_col]] <- pred[["lwr"]]
  df
}

#' Fit general model to data
#'
#' Used within `predict_lme4()`, this function fits the model to the data
#' frame, working whether the model is being fit across the entire data frame or
#' being fit to each group individually. Data is filtered prior to fitting,
#' model(s) are fit, and then fitted values are generated on the original.
#'
#' If fitting models individually to each group, `mdl` will never be returned, as
#' as these are instead a large group of models. Otherwise, a list of `mdl` and `df`
#' is returned and used within `predict_general_mdl()`.
#'
#' @inheritParams predict_lme4
#' @inheritParams fit_general_model
#'
#' @return List of `mdl` (fitted model) and `df` (data frame with fitted values
#'     and confidence bounds generated from the model).
fit_lme4_model <- function(df,
                           model,
                           formula,
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
  # Filter data for modeling
  if (!group_models) group_col_mdl <- NULL else group_col_mdl <- group_col

  data <- get_model_data(df = df,
                         formula_vars = formula_vars,
                         test_col = test_col,
                         group_col = group_col_mdl,
                         filter_na = filter_na)

  if (group_models) {

    # Split data frames
    data <- dplyr::group_by(data, .data[[group_col]]) %>%
      dplyr::group_split()

    df <- dplyr::group_by(df, .data[[group_col]]) %>%
      dplyr::group_split()

    # Build and apply models

    df <- purrr::map2_dfr(data, df, function(x, y) {
      mdl <- model(formula = formula,
                   data = x,
                   ...)
      predict_lme4_data(df = y,
                        model = mdl,
                        pred_col = pred_col,
                        upper_col = upper_col,
                        lower_col = lower_col)
    })

    mdl <- NULL # not returning all models together for grouped models
  } else { # single model fitting
    mdl <- model(formula = formula,
                 data = data,
                 ...)

    # don't predict data if only returning model
    if (ret == "mdl") {
      df <- NULL
    } else {
      df <- predict_lme4_data(df = df,
                              model = mdl,
                              pred_col = pred_col,
                              upper_col = upper_col,
                              lower_col = lower_col)
    }
  }

  # use error correction if applicable
  if (ret != "mdl") {
    df <- error_correct_fn(df = df,
                           response = formula_vars[1],
                           group_col = group_col,
                           sort_col = sort_col,
                           sort_descending = sort_descending,
                           pred_col = pred_col,
                           upper_col = upper_col,
                           lower_col = lower_col,
                           test_col = test_col,
                           error_correct = error_correct,
                           error_correct_cols = error_correct_cols,
                           shift_trend = shift_trend)
  }

  list(df = df, mdl = mdl)
}
