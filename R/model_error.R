#' Get modeling error from a data frame
#'
#' `model_error()` calculates modeling error using observed and fitted values from
#' the data frame. If `test_col` is provided, the error is only calculated on
#' observations that were excluded from modeling for test purpose. Otherwise,
#' the error is calculated for all non-missing values.
#'
#' The error metrics generated from `model_error()` are the following:
#'
#' * [RMSE](https://en.wikipedia.org/wiki/Root-mean-square_deviation): root mean squared error
#' * [MAE](https://en.wikipedia.org/wiki/Mean_absolute_error): mean absolute error
#' * [MdAE](https://scikit-learn.org/stable/modules/model_evaluation.html#median-absolute-error): median absolute error
#' * [MASE](https://en.wikipedia.org/wiki/Mean_absolute_scaled_error): mean absolute scaled error.
#'     Only calculated if `test_col` is provided, as it is test error scaled by in-sample error.
#' * CBA: confidence bound accuracy, % of observations lying within the confidence bounds.
#'     Should be very near to 95%. Only calculated if both `pred_upper_col` and `pred_lower_col`
#'     are provided.
#' * [R2](https://en.wikipedia.org/wiki/Coefficient_of_determination): R-squared
#'     or coefficient of determination. Calculated only on test values if `test_col`
#'     is provided. Due to the variety of models available within augury, as well
#'     as the `predict_..._avg_trend()` functions, adjusted R-squared is not
#'     currently available.
#' * [COR](https://en.wikipedia.org/wiki/Pearson_correlation_coefficient): Pearson
#'     correlation coefficient of fitted values to observations. Useful as a measure
#'     of general trend matching beyond the point error measurements used above. If
#'     `group_col` provided, correlation coefficients are calculated within each
#'     group and the average across all groups is returned. Calculated on all data,
#'     but be careful in interpreting when applied to non-time series data.
#' * RMChE: root mean change error. Since the GPW13 infilling and projections are designed
#'     to estimate change over time, RMChE measures the accuracy of this change. It is
#'     calculated as the difference between observed change between two time periods and
#'     predicted change across those same time periods. If `test_period` is `NULL`, this is
#'     the beginning and end of each group from `group_col`, sorted by `sort_col`. If
#'     `test_period` is provided as an integer `n`, then instead it is calculated comparing
#'     change between the end and `n` periods prior. `test_period_flexibility` says
#'     whether or not to calculate the change if the full length of the series is less
#'     than `test_period`. If `TRUE`, then it again compares change between the beginning
#'     and end of the series for that group.
#'
#' @inheritParams predict_general_mdl
#' @param response Column name of response variable.
#'
#' @return A named vector of errors: RMSE, MAE, MdAE, MASE, CBA, R2, COR and RMChE.
#'
#' @export
model_error <- function(df,
                        response,
                        test_col = NULL,
                        test_period = NULL,
                        test_period_flex = FALSE,
                        group_col = NULL,
                        sort_col = NULL,
                        sort_descending = FALSE,
                        pred_col = "pred",
                        pred_upper_col = "pred_upper",
                        pred_lower_col = "pred_lower") {
  # Group and arrange data if necessary

  df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group_col)))
  if (!is.null(sort_col)) {
    if (sort_descending) {
      fn <- dplyr::desc
    } else {
      fn <- NULL
    }
    df <- dplyr::arrange(df,
                         dplyr::across(dplyr::all_of(sort_col), fn),
                         .by_group = TRUE)
  }

  # If test_col being used, filter to that and calculate in-sample MASE denominator
  if (!is.null(test_col)) {
    train_df <- df %>%
      dplyr::mutate(!!sym(response) := ifelse(.data[[test_col]],
                                              NA,
                                              .data[[response]]),
                    "temp_pred_mase" := .data[[response]]) %>%
      tidyr::fill("temp_pred_mase") %>%
      dplyr::mutate("temp_pred_mase" := dplyr::lag(.data[["temp_pred_mase"]]),
                    "temp_pred_mase" := abs(.data[[response]] - .data[["temp_pred_mase"]])) %>%
      dplyr::filter(!is.na(.data[[response]]))

    mase_norm <- mean(train_df[["temp_pred_mase"]], na.rm = TRUE)

    df <- dplyr::filter(df, .data[[test_col]])
  } else { # without test column, just remove missing response variables
    df <- dplyr::filter(df, !is.na(.data[[response]]))
  }

  x <- df %>%
    dplyr::ungroup() %>%
    dplyr::mutate("diff" := .data[[pred_col]] - .data[[response]],
                  "diff_mean" := .data[[response]] - mean(.data[[response]], na.rm = T),
                  "diff_sqr" := .data[["diff"]] ^ 2,
                  "diff_mean_sqr" := .data[["diff_mean"]] ^ 2,
                  "diff_abs" := abs(.data[["diff"]])) %>%
    dplyr::summarize("RMSE" := sqrt(mean(.data[["diff_sqr"]], na.rm = TRUE)),
                     "MAE" := mean(.data[["diff_abs"]], na.rm = TRUE),
                     "MdAE" := stats::median(.data[["diff_abs"]], na.rm = TRUE),
                     "MASE" := if (!is.null(test_col)) .data[["MAE"]] / mase_norm else NA_real_,
                     "CBA" := if (!is.null(pred_upper_col) && !is.null(pred_lower_col)) sum(.data[[response]] <= .data[[pred_upper_col]] & .data[[response]] >= .data[[pred_lower_col]], na.rm = TRUE) / dplyr::n() else NA_real_,
                     "R2" := 1 - (sum(.data[["diff_sqr"]]) / sum(.data[["diff_mean_sqr"]])),
                     .groups = "drop")

  # Calculate COR and RMChE separately in case it's by group
  x_grped <- df %>%
    dplyr::summarize("COR" := suppressWarnings(stats::cor(.data[[pred_col]], .data[[response]], use = "na.or.complete")),
                     "RMChE" := calculate_sq_ch(.data[[response]], .data[[pred_col]], test_period, test_period_flex),
                     .groups = "drop") %>%
    dplyr::summarize("COR" := mean(.data[["COR"]],
                                   na.rm = TRUE),
                     "RMChE" := sqrt(mean(.data[["RMChE"]],
                                          na.rm = T)),
                     .groups = "drop")

  c(unlist(x), unlist(x_grped))
}

#' Calculate change error
#'
#' For use to calculate RMChE, this calculates the squared change error between observed and predicted columns.
#'
#' @inheritParams model_error
#' @param response Observations to test.
#' @param pred Predictions to test.
calculate_sq_ch <- function(response, pred, test_period, test_period_flex) {
  b <- length(response)
  if (is.null(test_period)) {
    a <- 1
  } else if (test_period < b) {
    a <- b - test_period
  } else if (test_period_flex) {
    a <- 1
  } else {
    return(NA_real_)
  }
  chg_er <- (response[b] - response[a]) - (pred[b] - pred[a])
  chg_er^2
}

