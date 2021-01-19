#' Use mean error to correct predictions
#'
#' @inherit predict_general_mdl params return
#' @param response Column name of response variable.
error_correct_fn <- function(df,
                             response,
                             pred_col,
                             upper_col,
                             lower_col,
                             error_correct,
                             error_correct_cols) {
  if (error_correct) {
    df <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(error_correct_cols))) %>%
      dplyr::mutate("temp_error" := .data[[response]] - .data[[pred_col]],
                    "temp_error" := mean(.data[["temp_error"]], na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate("temp_error" := ifelse(is.na(.data[["temp_error"]]),
                                           mean(.data[["temp_error"]], na.rm = TRUE),
                                           .data[["temp_error"]]),
                    !!sym(pred_col) := .data[[pred_col]] + .data[["temp_error"]],
                    !!sym(upper_col) := .data[[upper_col]] + .data[["temp_error"]],
                    !!sym(lower_col) := .data[[lower_col]] + .data[["temp_error"]])
  }
  df
}
