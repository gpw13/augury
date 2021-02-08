#' Use a time series model to infill and project data
#'
#' `predict_forecast()` uses the forecast package's [forecast::forecast()] methods
#' to generate predictions on time series data. These use the longest contiguous
#' observed values to forecast out a certain number periods. This function
#' automatically detects the latest observed values and the number of missing
#' values to forecast, and runs the provided forecasting function on the
#' observed data series.
#'
#' @param forecast_function An R function that outputs a forecast object coming from the
#'     forecast package. You can directly pass [forecast::forecast()] to the
#'     function, or you can pass other wrappers to it such as [forecast::holt()] or
#'     [forecast::ses()].
#' @param response Column name of response variable to be used as the input to the
#'     forecast function.
#' @param sort_col Column name of column to arrange data by in `dplyr::arrange()`,
#'     prior to filtering for latest contiguous time series and producing the
#'     forecast.
#' @param ... Additional arguments passed to the forecast function.
#'
#' @inherit predict_general_mdl params return
#'
#' @export
predict_forecast <- function(df,
                             forecast_function,
                             response,
                             ...,
                             ret = c("df", "all", "error", "model"),
                             scale = NULL,
                             probit = TRUE,
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
                             replace_obs = c("missing", "all", "none")) {
  # Assertions and error checking
  assert_df(df)
  assert_function(forecast_function)
  assert_columns(df, response, test_col, group_col, sort_col, type_col, source_col)
  ret <- rlang::arg_match(ret)
  assert_test_col(df, test_col)
  assert_string(pred_col, 1)
  assert_string(upper_col, 1)
  assert_string(lower_col, 1)
  filter_na <- rlang::arg_match(filter_na)
  assert_string(types, 3)
  assert_string(source, 1)
  replace_obs <- rlang::arg_match(replace_obs)

  if (!is.null(scale)) {
    df <- scale_transform(df, response, scale = scale)
  }

  if (probit) {
    df <- probit_transform(df, response)
  }

  mdl_df <- fit_forecast_model(df = df,
                               forecast_function = forecast_function,
                               response = response,
                               ...,
                               test_col = test_col,
                               group_col = group_col,
                               group_models = group_models,
                               sort_col = sort_col,
                               sort_descending = sort_descending,
                               pred_col = pred_col,
                               upper_col = upper_col,
                               lower_col = lower_col,
                               filter_na = filter_na,
                               ret = ret)

  mdl <- mdl_df[["mdl"]]
  df <- mdl_df[["df"]]

  if (ret == "model") {
    return(mdl)
  }

  # Untransform variables
  if (probit) {
    df <- probit_transform(df,
                           c(response,
                             pred_col,
                             upper_col,
                             lower_col),
                           inverse = TRUE)
  }

  # Unscale variables
  if (!is.null(scale)) {
    df <- scale_transform(df,
                          c(response,
                            pred_col,
                            upper_col,
                            lower_col),
                          scale = scale,
                          divide = FALSE)
  }

  # Get error if being returned
  if (ret %in% c("all", "error")) {
    err <- model_error(df = df,
                       response = response,
                       test_col = test_col,
                       group_col = group_col,
                       sort_col = sort_col,
                       sort_descending,
                       pred_col = pred_col,
                       upper_col = upper_col,
                       lower_col = lower_col)

    if (ret == "error") {
      return(err)
    }
  }

  # Merge predictions into observations
  df <- merge_prediction(df = df,
                         response = response,
                         group_col = group_col,
                         sort_col = sort_col,
                         sort_descending = sort_descending,
                         pred_col = pred_col,
                         upper_col = upper_col,
                         lower_col = lower_col,
                         test_col = test_col,
                         type_col = type_col,
                         types = types,
                         source_col = source_col,
                         source = source,
                         replace_obs = replace_obs,
                         error_correct = FALSE,
                         error_correct_cols = NULL)

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
#' `predict_forecast_data()` generates a prediction vector from a forecast object
#' and full data frame, putting this prediction back into the data frame.
#'
#' @inheritParams predict_general_mdl
#' @param forecast_obj Object of class `forecast` that is output from the `forecast::`
#'     family of functions.
#'
#' @return A data frame.
predict_forecast_data <- function(df,
                                  forecast_obj,
                                  pred_col,
                                  upper_col,
                                  lower_col) {
  x <- as.numeric(forecast_obj[["mean"]])
  x_len <- length(x)
  na_len <- nrow(df) - x_len # fill in NA for "pred" prior to the forecast
  df[[pred_col]] <- c(rep(NA_real_, na_len), x)
  df[[upper_col]] <- c(rep(NA_real_, na_len), get_forecast_bounds(forecast_obj, "upper"))
  df[[lower_col]] <- c(rep(NA_real_, na_len), get_forecast_bounds(forecast_obj, "lower"))
  df
}

#' @noRd
get_forecast_bounds <- function(x, bound) {
  df <- as.data.frame(x[[bound]])
  df[["95%"]]
}

#' Get data for forecast models
#'
#' Keep only the latest contiguous time series, dropping all other NA values
#' from the response variable. Removes test column variables first.
#'
#' @inheritParams predict_forecast
#'
#' @return A data series.
get_forecast_data <- function(df,
                              response,
                              sort_col,
                              sort_descending,
                              test_col) {
  if (!is.null(sort_col)) {
    if (sort_descending) {
      fn <- dplyr::desc
    } else {
      fn <- NULL
    }
    df <- dplyr::arrange(df, dplyr::across(dplyr::all_of(sort_col), fn), .by_group = TRUE)
  }

  if (!is.null(test_col)) {
    df[[response]][df[[test_col]]] <- NA_real_
  }

  trim_series(df[[response]])
}


#' Get latest data for forecasting
#'
#' Gets latest data for forecasting. It also gets the number of missing data
#' points to forecast.
#'
#' @param x Data series to reduce for forecasting
#'
#' @return Series with contiguous observations followed by NA values to forecast.
trim_series <- function(x) {
  na_x <- is.na(x)
  last_obs <- max(which(!na_x)) # latest observation
  missing <- which(na_x)
  start_from <- max(missing[missing < last_obs], -Inf) + 1 # find start of contiguous series
  if (is.infinite(start_from)) start_from <- 1
  x[start_from:length(x)]
}

#' Forecast data series
#'
#' Using series coming from `trim_series()`, it uses latest observed values
#' to forecast missing values.
#'
#' @param x Series to forecast, coming from `trim_series()`
#' @inheritParams predict_forecast
#'
#' @return Forecast model.
forecast_series <- function(x,
                            forecast_function,
                            ...) {
  na_x <- is.na(x)
  h <- sum(na_x)
  assert_h(h)
  x <- x[!na_x]
  forecast_function(x,
                    h = h,
                    ...)
}

#' Fit forecast model to data
#'
#' Used within `predict_forecast()`, this function fits the model to the data
#' frame, working whether the model is being fit across the entire data frame or
#' being fit to each group individually. Data is filtered prior to fitting,
#' model(s) are fit, and then fitted values are generated on the original.
#'
#' If fitting models individually to each group, `mdl` will never be returned, as
#' as these are instead a large list of models. Otherwise, a list of `mdl` and `df`
#' is returned and used within `predict_inla()`.
#'
#' @inheritParams predict_forecast
#' @inheritParams fit_general_model
#'
#' @return List of `mdl` (fitted model) and `df` (data frame with fitted values
#'     and confidence bounds generated from the model).
fit_forecast_model <- function(df,
                               forecast_function,
                               response,
                               ...,
                               test_col,
                               group_col,
                               group_models,
                               sort_col,
                               sort_descending,
                               pred_col,
                               upper_col,
                               lower_col,
                               filter_na,
                               ret) {

  if (!group_models) {
    # Filter data for modeling
    x <- get_forecast_data(df = df,
                           response = response,
                           sort_col = sort_col,
                           sort_descending = sort_descending,
                           test_col = test_col)

    # Build model
    mdl <- forecast_series(x,
                           forecast_function,
                           ...)

    if (ret == "model") {
      df <- NULL
    } else {
      # Get model predictions
      df <- predict_forecast_data(df = df,
                                  forecast_obj = mdl,
                                  pred_col = pred_col,
                                  upper_col = upper_col,
                                  lower_col = lower_col)
    }
  } else {
    # map by group
    df_list <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group_col))) %>%
      dplyr::group_split()

    df <- purrr::map_dfr(df_list, function(df) {
      x <- get_forecast_data(df = df,
                             response = response,
                             sort_col = sort_col,
                             sort_descending = sort_descending,
                             test_col = test_col)
      mdl <- forecast_series(x,
                             forecast_function,
                             ...)
      predict_forecast_data(df = df,
                            forecast_obj = mdl,
                            pred_col = pred_col,
                            upper_col = upper_col,
                            lower_col = lower_col)
    })

    mdl <- NULL
  }
  list(df = df, mdl = mdl)
}
