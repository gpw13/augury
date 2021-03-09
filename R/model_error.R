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
#'     Should be very near to 95%. Only calculated if both `upper_col` and `lower_col`
#'     are provided.
#' * [COR](https://en.wikipedia.org/wiki/Pearson_correlation_coefficient): Pearson
#'     correlation coefficient of fitted values to observations. Useful as a measure
#'     of general trend matching beyond the point error measurements used above. If
#'     `group_col` provided, correlation coefficients are calculated within each
#'     group and the average across all groups is returned. Calculated on all data,
#'     but be careful in interpreting when applied to non-time series data.
#' * [R2](https://en.wikipedia.org/wiki/Coefficient_of_determination): R-squared
#'     or coefficient of determination. Calculated only on test values if `test_col`
#'     is provided. Due to the variety of models available within augury, as well
#'     as the `predict_..._avg_trend()` functions, adjusted R-squared is not
#'     currently available.
#'
#' @inheritParams predict_general_mdl
#' @param response Column name of response variable.
#' @param k Number of predictors in the model.
#'
#' @return A named vector of errors: RMSE, MAE, MdAE, MASE, CBA, and COR.
#'
#' @export
model_error <- function(df,
                        response,
                        test_col = NULL,
                        group_col = NULL,
                        sort_col = NULL,
                        sort_descending = FALSE,
                        pred_col = "pred",
                        upper_col = "upper",
                        lower_col = "lower") {
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
                     "CBA" := if (!is.null(upper_col) && !is.null(lower_col)) sum(.data[[response]] <= .data[[upper_col]] & .data[[response]] >= .data[[lower_col]], na.rm = TRUE) / dplyr::n() else NA_real_,
                     "R2" := 1 - (sum(.data[["diff_sqr"]]) / sum(.data[["diff_sqr_mean"]])),
                     .groups = "drop")

  # Calculate COR separately in case it's by group
  x_cor <- df %>%
    dplyr::summarize("COR" := stats::cor(.data[[pred_col]], .data[[response]], use = "complete.obs"),
                     .groups = "drop") %>%
    dplyr::summarize("COR" := mean(.data[["COR"]],
                                   na.rm = TRUE),
                     .groups = "drop")

  c(unlist(x), unlist(x_cor))
}
