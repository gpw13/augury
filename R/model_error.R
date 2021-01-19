
#' Get modeling error from a data frame
#'
#' `model_error()` calculates modeling error using observed and fitted values from
#' the data frame. If `test_col` is provided, the error is only calculated on
#' observations that were excluded from modeling for test purpose. Otherwise,
#' the error is calculated for all non-missing values.
#'
#' @inheritParams predict_general_mdl
#' @param response Column name of response variable.
#'
#' @return A named vector of errors: RMSE, MSE, and MASE.
#'
#' @export
model_error <- function(df,
                        response,
                        pred_col,
                        test_col) {
  if (!is.null(test_col)) {
    df <- dplyr::filter(df, .data[[test_col]])
  } else {
    df <- dplyr::filter(df, !is.na(.data[[response]]))
  }

  x <- df %>%
    dplyr::mutate("diff" := .data[[pred_col]] - .data[[response]],
                  "diff_sqr" := .data[["diff"]] ^ 2,
                  "diff_abs" := abs(.data[["diff"]])) %>%
    dplyr::summarize("mse" := mean(.data[["diff_sqr"]]),
                     "rmse" := sqrt(.data[["mse"]]),
                     "mae" := mean(.data[["diff_abs"]]))

  unlist(x)
}
