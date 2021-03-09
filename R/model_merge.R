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
                             group_col,
                             sort_col,
                             sort_descending,
                             pred_col,
                             upper_col,
                             lower_col,
                             test_col,
                             type_col,
                             types,
                             source_col,
                             source,
                             replace_obs) {
  if (replace_obs != "none") {
    if (!is.null(type_col)) {
      df <- dplyr::group_by(df, dplyr::across(group_col))
    }

    # put types for missing values before, between, and after the included data (types 1, 2, 3)

    if (!is.null(type_col)) {
      if (!is.null(sort_col)) {
        if (sort_descending) {
          fn <- dplyr::desc
        } else {
          fn <- NULL
        }
        df <- dplyr::arrange(df, dplyr::across(sort_col, fn), .by_group = TRUE)
      }
      df <- df %>%
        dplyr::mutate(
          !!sym(type_col) := dplyr::case_when(
            is.na(.data[[pred_col]]) ~ NA_character_,
            is.na(.data[[response]]) & dplyr::row_number() <= min(which(!is.na(.data[[response]])), Inf) ~ types[1],
            is.na(.data[[response]]) & dplyr::row_number() > max(which(!is.na(.data[[response]])), -Inf) ~ types[3],
            !is.na(.data[[response]]) ~ .data[[type_col]],
            TRUE ~ types[2]
          ))
    }

    # put source for missing values, where applicable
    if (!is.null(source_col)) {
      df[[source_col]] <- ifelse(is.na(df[[response]]) & !is.na(df[[pred_col]]), source, df[[source_col]])
    }

    # replace data with predicted values
    if (replace_obs == "missing") {
      df[[response]] <- ifelse(is.na(df[[response]]), df[[pred_col]], df[[response]])
    } else {
      df[[response]] <- df[[pred_col]]
    }
  }
  dplyr::ungroup(df)
}
