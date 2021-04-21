#' Use a generic R model to infill and project data
#'
#' `predict_general_mdl()` uses a general model object from R to fit a model and
#' use that model to infill and project the dependent variable. It is flexible
#' to allow for many general models to be used through the function. However,
#' they need to fit certain criteria:
#' * The model accepts `formula` and `data` arguments. All other arguments can be
#'     passed anonymously through  `...`.
#' * The returned object passed through [stats::family()] returns an inverse
#'     link function in its list as `linkinv`.
#' * Must have a `predict.model` generic that accepts the `se.fit = TRUE` argument
#'     and returns confidence intervals.
#'
#' As example, [stats::lm()] and [stats::glm()]  fit the
#' above criteria and convenient wrappers for those models are
#' provided in augury, but additional model functions can be used in
#' `predict_general_mdl()` if they fit the criteria.
#'
#' The function also allows for inputting of data type and source information
#' directly into the data frame if the `type_col` and `source_col` are specified
#' respectively.
#'
#' @param df Data frame of model data.
#' @param model An R function that outputs a model object with a `predict.model` generic,
#'     where [stats::family()] contains an inverse link function `linkinv` and
#'     `predict.model()` accepts the `se.fit = TRUE` argument and returns confidence
#'     intervals. This includes [stats::lm], [stats::glm], and [lme4::lmer].
#' @param formula A formula that will be supplied to the model, such as `y~x`.
#' @param ... Other arguments passed to the model function.
#' @param ret Character vector specifying what values the function returns. Defaults
#'     to returning a data frame, but can return a vector of model error, the
#'     model itself or a list with all 3 as components.
#' @param scale Either `NULL` or a numeric value. If a numeric value is provided,
#'     the response variable is scaled by the value passed to scale prior to model
#'     fitting and prior to any probit transformation, so can be used to put the
#'     response onto a 0 to 1 scale. Scaling is done by dividing the response by
#'     the scale and using the [scale_transform()] function. The response, as well
#'     as the fitted values and confidence bounds are unscaled prior to error
#'     calculation and returning to the user.
#' @param probit Logical value on whether or not to probit transform the response
#'     prior to model fitting. Probit transformation is performed after any scaling
#'     determined by `scale` but prior to model fitting. The response, as well as
#'     the fitted values and confidence bounds are untransformed prior to error
#'     calculation and returning to the user.
#' @param test_col Name of logical column specifying which response values to remove
#'     for testing the model's predictive accuracy. If `NULL`, ignored. See [model_error()]
#'     for details on the methods and metrics returned.
#' @param test_period Length of period to test for RMChE. If `NULL`, beginning and end
#'     points of each group in `group_col` are compared. Otherwise, `test_period` must
#'     be set to an integer `n` and for each group, comparisons are made between
#'     the end point and `n` periods prior.
#' @param test_period_flex Logical value indicating if `test_period` is less than
#'    the full length of the series, should change error still be calculated for that
#'    point. Defaults to `FALSE`.
#' @param group_col Column name(s) of group(s) to use in [dplyr::group_by()] when
#'     supplying type, calculating mean absolute scaled error on data involving
#'     time series, and if `group_models`, then fitting and predicting models too.
#'     If `NULL`, not used. Defaults to `"iso3"`.
#' @param group_models Logical, if `TRUE`, fits and predicts models individually onto
#'     each `group_col`. If `FALSE`, a general model is fit across the entire data
#'     frame.
#' @param obs_filter String value of the form "`logical operator` `integer`"
#'     that specifies the number of observations required to fit the model and
#'     replace observations with predicted values. This is done in
#'     conjunction with `group_col`. So, if `group_col = "iso3"` and
#'     `obs_filter = ">= 5"`, then for this model, predictions will only be used
#'     for `iso3` vales that have 5 or more observations. Possible logical operators
#'     to use are `>`, `>=`, `<`, `<=`, `==`, and `!=`.
#'
#'     If `group_models = FALSE`, then `obs_filter` is only used to determine when
#'     predicted values replace observed values but **is not** used to restrict values
#'     from being used in model fitting. If `group_models = TRUE`, then a model
#'     is only fit for a group if they meet the `obs_filter` requirements. This provides
#'     speed benefits, particularly when running INLA time series using `predict_inla()`.
#' @param sort_col Column name(s) to use to [dplyr::arrange()] the data prior to
#'     supplying type and calculating mean absolute scaled error on data involving
#'     time series. If `NULL`, not used. Defaults to `"year"`.
#' @param sort_descending Logical value on whether the sorted values from `sort_col`
#'     should be sorted in descending order. Defaults to `FALSE`.
#' @param pred_col Column name to store predicted value.
#' @param upper_col Column name to store upper bound of confidence interval.
#' @param lower_col Column name to store lower bound of confidence interval.
#' @param filter_na Character value specifying how, if at all, to filter `NA`
#'     values from the dataset prior to applying the model.  By default, all
#'     observations with missing values are removed, although it can also remove
#'     rows only if they have missing dependent or independent variables, or no
#'     filtering at all.
#' @param type_col Column name specifying data type.
#' @param types Vector of length 3 that provides the type to provide to data
#'     produced in the model. These values are only used to fill in type values
#'     where the dependent variable is missing. The first value is given to missing
#'     observations that precede the first observation, the second to those after
#'     the last observation, and the third for those following the final observation.
#' @param source_col Column name containing source information for the data frame.
#'     If provided, the argument in `source` is used to fill in where predictions
#'     have filled in missing data.
#' @param source Source to add to missing values.
#' @param replace_obs Character value specifying how, if at all, observations should
#'     be replaced by fitted values. Defaults to replacing only missing values,
#'     but can be used to replace all values or none.
#' @param error_correct Logical value indicating whether or not whether mean error
#'     should be used to adjust predicted values. If `TRUE`, the mean error between
#'     observed and predicted data points will be used to adjust predictions. If
#'     `error_correct_cols` is not `NULL`, mean error will be used within those
#'     groups instead of overall mean error.
#' @param error_correct_cols Column names of data frame to group by when applying
#'     error correction to the predicted values.
#' @param shift_trend Logical value specifying whether or not to shift predictions
#'     so that the trend matches up to the last observation. If `error_correct` and
#'     `shift_trend` are both `TRUE`, `shift_trend` takes precedence.
#'
#' @return Depending on the value passed to `ret`, either a data frame with
#'     predicted data, a vector of errors from [model_error()], a fitted model, or a list with all 3.
#'
#' @export
predict_general_mdl <- function(df,
                                model,
                                formula,
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
                                upper_col = "upper",
                                lower_col = "lower",
                                filter_na = c("all", "response", "predictors", "none"),
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
  assert_model(model)
  formula_vars <- parse_formula(formula)
  assert_columns(df, formula_vars, test_col, group_col, sort_col, type_col, source_col)
  assert_group_models(group_col, group_models)
  response <- formula_vars[1]
  assert_columns_unique(response, pred_col, upper_col, lower_col, test_col, group_col, sort_col, type_col, source_col)
  ret <- rlang::arg_match(ret)
  assert_test_col(df, test_col)
  assert_string(pred_col, 1)
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

  mdl_df <- fit_general_model(df = df,
                              model = model,
                              formula = formula,
                              ...,
                              formula_vars = formula_vars,
                              test_col = test_col,
                              group_col = group_col,
                              group_models = group_models,
                              obs_filter = obs_filter,
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
                         obs_filter = obs_filter,
                         sort_col = sort_col,
                         sort_descending = sort_descending,
                         pred_col = pred_col,
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

#' Generate prediction from model object
#'
#' `predict_general_data()` generates a prediction vector from a model object and full
#' data frame, putting this prediction back into the data frame.
#'
#' @inheritParams predict_general_mdl
#' @return A data frame.
predict_general_data <- function(df,
                                 model,
                                 pred_col,
                                 upper_col,
                                 lower_col) {
  inv_link <- stats::family(model)[["linkinv"]]
  pred <- stats::predict(model, newdata = df, se.fit = TRUE)
  x <- pred[["fit"]]
  se <- pred[["se.fit"]]
  df[[pred_col]] <- inv_link(x)
  df[[upper_col]] <- inv_link(x + 2 * se)
  df[[lower_col]] <- inv_link(x - 2 * se)
  df
}

#' Fit general model to data
#'
#' Used within `predict_general_mdl()`, this function fits the model to the data
#' frame, workingw hether the model is being fit across the entire data frame or
#' being fit to each group individually. Data is filtered prior to fitting,
#' model(s) are fit, and then fitted values are generated on the original.
#'
#' If fitting models individually to each group, `mdl` will never be returned, as
#' as these are instead a large group of models. Otherwise, a list of `mdl` and `df`
#' is returned and used within `predict_general_mdl()`.
#'
#' @inheritParams predict_general_mdl
#' @param formula_vars Variables included in the model formula, generated by
#'     `all.vars(formula)`.
#'
#' @return List of `mdl` (fitted model) and `df` (data frame with fitted values
#'     and confidence bounds generated from the model).
fit_general_model <- function(df,
                              model,
                              formula,
                              ...,
                              formula_vars,
                              test_col,
                              group_col,
                              group_models,
                              obs_filter,
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
      obs_check <- dplyr::filter(y, eval(parse(text = obs_filter)))
      if (nrow(obs_check) == 0) {
        mdl <- model(formula = formula,
                     data = x,
                     ...)

        predict_general_data(df = y,
                             model = mdl,
                             pred_col = pred_col,
                             upper_col = upper_col,
                             lower_col = lower_col)
      } else {
        y
      }
    })
    df <- augury_add_columns(df, c(pred_col, lower_col, upper_col))

    mdl <- NULL # not returning all models together for grouped models
  } else { # single model fitting

    mdl <- model(formula = formula,
                 data = data,
                 ...)
    if (ret == "mdl") {
      df <- NULL
    } else {
      df <- dplyr::group_by(df, dplyr::across(group_col)) %>%
        dplyr::mutate("augury_temp_obs_check" := eval(parse(text = obs_filter))) %>%
        dplyr::group_by(.data[["augury_temp_obs_check"]]) %>%
        dplyr::group_modify(function(x, ...) {
          if (!unique(x[["augury_temp_obs_check"]])) {
            x <- predict_general_data(df = x,
                                      model = mdl,
                                      pred_col = pred_col,
                                      upper_col = upper_col,
                                      lower_col = lower_col)
          }
          dplyr::select(x, -"augury_temp_obs_check")
        },
        .keep = TRUE) %>%
        dplyr::ungroup() %>%
        dplyr::select(-"augury_temp_obs_check")
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
