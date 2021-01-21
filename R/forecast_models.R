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
#' As example, [stats::lm()], [stats::glm()], and [lme4::lmer()] all fit the
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
#' @param test_col Name of logical column specifying which response values to remove
#'     for testing the model's predictive accuracy. If `NULL`, ignored.
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
#' @param type_group Column name(s) of group(s) to use in [dplyr::group_by()] when
#'     supplying type.
#' @param type_sort Column name to use to [dplyr::arrange()] the data prior to
#'     supplying type.
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
#'
#' @return Depending on the value passed to `ret`, either a data frame with
#'     predicted data, a vector of errors, a fitted model, or a list with all 3.
#'
#' @export
predict_general_mdl <- function(df,
                                model,
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
  # Assertions and error checking
  assert_df(df)
  assert_model(model)
  formula_vars <- parse_formula(formula)
  assert_columns(df, formula_vars, test_col, type_col, source_col)
  ret <- rlang::arg_match(ret)
  assert_test_col(df, test_col)
  assert_string_l1(pred_col)
  assert_string_l1(upper_col)
  assert_string_l1(lower_col)
  filter_na <- rlang::arg_match(filter_na)
  assert_string_l1(type_col)
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
  mdl <- model(formula = formula,
               data = data,
               ...)

  if (ret == "model") {
    return(mdl)
  }

  # Get model predictions
  df <- predict_data(df,
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

#' Generate prediction from model object
#'
#' `predict_data()` generates a prediction vector from a model object and full
#' data frame, putting this prediction back into the data frame.
#'
#' @inheritParams predict_general_mdl
#' @return A data frame.
predict_data <- function(df,
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
