#' Produces averaged data frame that can then be passed for modelling.
#'
#' @inheritParams predict_inla_avg_trend
#'
#' @param cols Name of numeric column(s) to take average of.
#'
#' @return Data frame of `cols` averaged across `average_cols`
get_average_df <- function(df,
                           cols,
                           average_cols,
                           weight_col = NULL) {
  df <- df %>%
    dplyr::group_by(dplyr::across(average_cols))

  if (!is.null(weight_col)) {
    df <- dplyr::summarize(df,
                           dplyr::across(cols,
                                         ~stats::weighted.mean(.x, .data[[weight_col]], na.rm = T)),
                           .groups = "drop")
  } else {
    df <- dplyr::summarize(df,
                           dplyr::across(cols,
                                         ~mean(.x, na.rm = T)),
                           .groups = "drop")
  }
  df
}

#' Get variables that need to be averaged from formula.
#'
#' Pulls out variables not being used for grouping that will be averaged, checks
#' that they are numeric, and returns them.
#'
#' @inheritParams predict_inla_avg_trend
#' @inheritParams fit_general_model
get_formula_avg_cols <- function(df,
                                 formula_vars,
                                 average_cols) {
  cols <- formula_vars[!(formula_vars %in% average_cols)]
  assert_numeric_cols_avg(cols, df)
  cols
}

#' Merge average df with predictions with original data frame
#'
#' @inheritParams predict_inla_avg_trend
#' @inheritParams merge_prediction
#' @param avg_df Data frame with average trends.
#'
#' @return Original data frame with new trend joined up.
merge_average_df <- function(avg_df,
                             df,
                             response,
                             average_cols,
                             group_col,
                             sort_col,
                             pred_col,
                             upper_col,
                             lower_col) {
  # group average data
  # don't use sort_col in grouping for wrangling
  avg_df <- dplyr::group_by(avg_df,
                            dplyr::across(average_cols[!(average_cols %in% sort_col)]))

  # create predictions
  # if sort_col provided, use trend for `group_col` that have data
  # otherwise no trend
  if (!is.null(sort_col)) {
    avg_df <- avg_df %>%
      dplyr::arrange(.data[[sort_col]], .by_group = TRUE) %>%
      dplyr::mutate(!!sym(paste0(pred_col, "_trend")) := .data[[pred_col]] - dplyr::lag(.data[[pred_col]]),
                    !!sym(paste0(upper_col, "_trend")) := .data[[upper_col]] - .data[[pred_col]],
                    !!sym(paste0(lower_col, "_trend")) := .data[[lower_col]] - .data[[pred_col]]) %>%
      dplyr::select(average_cols,
                    pred_col,
                    upper_col,
                    lower_col,
                    paste0(c(pred_col, upper_col, lower_col), "_trend"))

    df <- df %>%
      dplyr::left_join(avg_df, by = average_cols) %>%
      dplyr::group_by(dplyr::across(group_col)) %>%
      dplyr::arrange(.data[[sort_col]], .by_group = TRUE) %>%
      dplyr::mutate("temp_fill_response" := temp_fill(.data[[response]]),
                    "temp_forward_trend" := forward_trend(.data[[response]], .data[["temp_fill_response"]], .data[[paste0(pred_col, "_trend")]]),
                    "temp_backward_trend" := backward_trend(.data[[response]], .data[["temp_fill_response"]], .data[[paste0(pred_col, "_trend")]]),
                    !!sym(pred_col) := dplyr::case_when(
                      all(is.na(.data[[response]])) ~ .data[[pred_col]],     # use entire pred for groups with no data
                      !is.na(.data[[response]]) ~ .data[[response]],         # use observed values where available
                      dplyr::row_number() > min(which(!is.na(.data[[response]])), Inf) ~ .data[["temp_forward_trend"]],
                      dplyr::row_number() < min(which(!is.na(.data[[response]])), Inf) ~ .data[["temp_backward_trend"]]
                    ),
                    !!sym(upper_col) := dplyr::case_when(
                      !is.na(.data[[response]]) ~ .data[[pred_col]],         # no bounds if value already present
                      TRUE ~ .data[[pred_col]] + .data[[paste0(upper_col, "_trend")]]          # otherwise, add to prediction column
                    ),
                    !!sym(lower_col) := dplyr::case_when(
                      !is.na(.data[[response]]) ~ .data[[pred_col]],         # no bounds if value already present
                      TRUE ~ .data[[pred_col]] - .data[[paste0(lower_col, "_trend")]]          # otherwise, subtract from prediction column
                    )) %>%
      dplyr::select(-c(paste0(c(pred_col, upper_col, lower_col), "_trend"),  # drop temporary columns
                       "temp_fill_response", "temp_forward_trend", "temp_backward_trend"))

  } else {

    # predictions here created with sort col
    # no trend is generated, just raw values of regional trends used
    # prediction kept always at that regional level
    avg_df <- dplyr::arrange(.by_group = TRUE) %>%
      dplyr::select(average_cols,
                    pred_col,
                    upper_col,
                    lower_col)

    df <- df %>%
      dplyr::left_join(avg_df, by = average_cols)
  }
  dplyr::ungroup(df)
}

#' Fills vector backwards and forward, for use prior to applying average trend
#'
#' @param x Vector to fill, typically response vector
temp_fill <- function(x) {
  if (all(is.na(x))) {
    x
  } else {
    zoo::na.locf(zoo::na.locf(x, na.rm = F), fromLast = TRUE)
  }
}

#' @noRd
forward_trend <- function(resp, resp_fill, trend) {
  cs_trend <- stats::ave(trend,
                      cumsum(is.na(resp) & !is.na(dplyr::lag(resp))),
                      FUN = cumsum)
  resp_fill + cs_trend
}

#' @noRd
backward_trend <- function(resp, resp_fill, trend) {
  cs_trend <- stats::ave(trend,
                         cumsum(is.na(dplyr::lag(resp)) & !is.na(resp)),
                         FUN = function(x) rev(cumsum(rev(x))))
  resp_fill - cs_trend
}
