#' Merge predicted data into data frame
#'
#' Merges predicted data into data frame. By default, does not replace observed
#' values with modeled data.
#'
#' @inheritParams predict_general_mdl
#' @param response Column name of response variable.
#'
#' @return A data frame.
merge_prediction <- function(df,
                             response,
                             group_col,
                             obs_filter,
                             sort_col,
                             sort_descending,
                             pred_col,
                             type_col,
                             types,
                             source_col,
                             source,
                             replace_obs) {
  if (replace_obs != "none") {
    # changing pred and response to both be real valued, in case one is integer
    df <- dplyr::mutate(df, dplyr::across(c(response, pred_col), as.numeric))

    df <- dplyr::group_by(df, dplyr::across(group_col))

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
            eval(parse(text = obs_filter)) ~ .data[[type_col]],
            is.na(.data[[pred_col]]) ~ NA_character_,
            is.na(.data[[response]]) & dplyr::row_number() <= min(which(!is.na(.data[[response]])), Inf) ~ types[1],
            is.na(.data[[response]]) & dplyr::row_number() > max(which(!is.na(.data[[response]])), -Inf) ~ types[3],
            !is.na(.data[[response]]) ~ .data[[type_col]],
            TRUE ~ types[2]
          ))
    }

    # put source for missing values, where applicable
    if (!is.null(source_col)) {
      df <- df %>%
        dplyr::mutate(!!sym(source_col) := dplyr::case_when(
          eval(parse(text = obs_filter)) ~ .data[[source_col]],
          is.na(.data[[response]]) & !is.na(.data[[pred_col]]) ~ !!source,
          TRUE ~ .data[[source_col]]
        ))
    }

    # replace data with predicted values
    if (replace_obs == "missing") {
      df <- df %>%
        dplyr::mutate(!!sym(response) := dplyr::case_when(
          eval(parse(text = obs_filter)) ~ .data[[response]],
          is.na(.data[[response]]) ~ .data[[pred_col]],
          TRUE ~ .data[[response]]
        ))
    } else {
      df <- df %>%
        dplyr::mutate(!!sym(response) := dplyr::case_when(
          eval(parse(text = obs_filter)) ~ .data[[response]],
          TRUE ~ .data[[pred_col]]
        ))
    }
  }

  dplyr::ungroup(df)
}
