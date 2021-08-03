#' Use mean error to correct predictions
#'
#' @inherit predict_general_mdl params return
#' @param response Column name of response variable.
error_correct_fn <- function(df,
                             response,
                             group_col,
                             sort_col,
                             sort_descending,
                             pred_col,
                             pred_upper_col,
                             pred_lower_col,
                             test_col,
                             error_correct,
                             error_correct_cols,
                             shift_trend) {
  if (error_correct & !shift_trend) {
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
                                           .data[["temp_error"]])) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(c(pred_col, pred_upper_col, pred_lower_col)),
                                  ~ .x + .data[["temp_error"]])) %>%
      dplyr::select(-"temp_error")
  }

  if (shift_trend) {
    df <- dplyr::group_by(df, dplyr::across(group_col))

    if (!is.null(sort_col)) {
      if (sort_descending) {
        fn <- dplyr::desc
      } else {
        fn <- NULL
      }
      df <- dplyr::arrange(df, dplyr::across(dplyr::all_of(sort_col), fn), .by_group = TRUE)
    }

    df <- df %>%
      dplyr::mutate("temp_error" := if (is.null(test_col)) .data[[response]] else ifelse(.data[[test_col]], NA_real_, .data[[response]]),
                    "temp_error" := match_trend(.data[["temp_error"]], .data[[pred_col]])) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(c(pred_col, pred_upper_col, pred_lower_col)),
                                  ~ .x + .data[["temp_error"]])) %>%
      dplyr::select(-"temp_error") %>%
      dplyr::ungroup()
  }

  df
}

#' @noRd
match_trend <- function(response, pred) {
  nonmissing <- !is.na(response)
  if (all(!nonmissing)) {
    return(0)
  }

  last_obs_idx <- max(which(nonmissing))
  last_obs <- response[last_obs_idx]
  last_pred <- pred[last_obs_idx]
  last_obs - last_pred
}

