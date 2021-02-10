#' Use mean error to correct predictions
#'
#' @inherit predict_general_mdl params return
#' @param response Column name of response variable.
error_correct_fn <- function(df,
                             response,
                             pred_col,
                             upper_col,
                             lower_col,
                             test_col,
                             error_correct,
                             error_correct_cols) {
  if (error_correct) {
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(error_correct_cols))) %>%
      dplyr::mutate("temp_error" := .data[[response]] - .data[[pred_col]])

    if (!is.null(test_col)) {
      df[["temp_error"]][df[[test_col]]] <- NA_real_
    }

    df <- df %>%
      dplyr::mutate("temp_error" := mean(.data[["temp_error"]], na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate("temp_error" := ifelse(is.na(.data[["temp_error"]]),
                                           mean(.data[["temp_error"]], na.rm = TRUE),
                                           .data[["temp_error"]]),
                    dplyr::across(dplyr::any_of(c(pred_col, upper_col, lower_col)),
                                                ~ .x + .data[["temp_error"]])) %>%
      dplyr::select(-"temp_error")
  }

  df
}
