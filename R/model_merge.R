#' Merge predicted data into data frame
#'
#' Merges predicted data into data frame. By default, does not replace observed
#' values with modeled data, although upper and lower bounds for the model always
#' included.
#'
#' @inheritParams predict_general_mdl
#' @param response Column name of response variable.
#'
#' @return A data frame.
merge_prediction <- function(df,
                             response,
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
                             error_correct_cols) {
  if (replace_obs != "none") {
    if (!is.null(type_col)) {
      df <- dplyr::group_by(df, dplyr::across(type_group))
    }

    # put types for missing values before, between, and after the included data (types 1, 2, 3)

    if (!is.null(type_col)) {
      df <- dplyr::arrange(df, .data[[type_sort]], .by_group = TRUE) %>%
        dplyr::mutate(
          !!sym(type_col) := dplyr::case_when(
            !is.na(.data[[response]]) ~ .data[[type_col]],
            .data[[type_sort]] <= min(.data[[type_sort]][!is.na(.data[[response]])]) ~ types[1],
            .data[[type_sort]] > max(.data[[type_sort]][!is.na(.data[[response]])]) ~ types[3],
            TRUE ~ types[2]
          ))
    }

    # put source for missing values, where applicable
    if (!is.null(source_col)) {
      df[[source_col]] <- ifelse(is.na(df[[response]]), source, df[[source_col]])
    }

    # use error correction if applicable
    df <- error_correct_fn(df,
                           response,
                           pred_col,
                           upper_col,
                           lower_col,
                           error_correct,
                           error_correct_cols)

    # replace data with predicted values
    if (replace_obs == "missing") {
      df[[response]] <- ifelse(is.na(df[[response]]), df[[pred_col]], df[[response]])
    } else {
      df[[response]] <- df[[pred_col]]
    }
  }
  dplyr::ungroup(df)
}
