#' Expand input data to make explicit missing values
#'
#' `expand_df()` is a wrapper around [tidyr::expand_grid()] and [dplyr::right_join()]
#' that can be used to make missing values explicit within a data frame prior to
#' it being passed to a `predict_...()` function.
#'
#' @param df Data frame.
#' @param ... Named vectors to pass to expand grid.
#' @param response Column name of response variables whose missing values will be
#'     infilled and projected, defaults to `"value"`.
#' @param keep_no_obs Logical value indicating whether or not to keep rows in the
#'     expanded data frame when there is no data. Defaults to `TRUE`. This is done based
#'     on the `group_col`, if provided.
#' @param keep_before_obs Logical value indicating when data is available, whether or not
#'     to keep rows in the expanded data frame that lie before the first observed point.
#'     Defaults to `FALSE`. This is done based on the `sort_col` and `group_col`, if provided.
#' @param join_covariates Logical value indicating whether or not to join the final
#'     expanded data frame to the [augury::covariates_df] data frame. If `TRUE`,
#'     `iso3` and `year` must be columns within the input `df`.
#' @inheritParams predict_general_mdl
#'
#' @return Expanded data frame with explicit missing values.
#' @export
expand_df <- function(df,
                      ...,
                      response = "value",
                      keep_no_obs = TRUE,
                      keep_before_obs = FALSE,
                      sort_col = "year",
                      sort_descending = FALSE,
                      group_col = "iso3",
                      join_covariates = FALSE) {
  dot_names <- names(list(...))
  assert_columns(df, dot_names)

  expanded_df <- tidyr::expand_grid(...) %>%
    dplyr::left_join(df, by = dot_names)

  expanded_df <- expand_df_filter(expanded_df,
                                  response,
                                  keep_no_obs,
                                  keep_before_obs,
                                  sort_col,
                                  sort_descending,
                                  group_col)

  expanded_df <- join_covariates_df(expanded_df, join_covariates)
  expanded_df
}

#' Filter `expand_df`
#'
#' Used within `expand_df()` to filter out the data based on the arguments `keep_no_obs`
#' and `keep_before_obs`.
#'
#' @inheritParams expand_df
#'
#' @return Filtered expanded data frame.
expand_df_filter <- function(df,
                             response,
                             keep_no_obs,
                             keep_before_obs,
                             sort_col,
                             sort_descending,
                             group_col) {
  if (!keep_no_obs || !keep_before_obs) {
    assert_columns(df, sort_col, group_col)
    df <- dplyr::group_by(df, dplyr::across(group_col))

    # put types for missing values before, between, and after the included data (types 1, 2, 3)

    if (!is.null(sort_col)) {
      if (sort_descending) {
        fn <- dplyr::desc
      } else {
        fn <- NULL
      }
      df <- dplyr::arrange(df, dplyr::across(sort_col, fn), .by_group = TRUE)
    }
    if (!keep_before_obs) {
      df <- df %>%
        dplyr::filter(dplyr::row_number() >= min(which(!is.na(.data[[response]])), Inf))
    }

    if (!keep_no_obs) {
      df <- df %>%
        dplyr::filter(!all(is.na(.data[[response]])))
    }
  }
  dplyr::ungroup(df)
}

#' Join data frame with covariates data frame
#'
#' Left joins a data frame with `augury::covariates_df`.
#'
#' @inheritParams expand_df
#'
#' @return Joined data frame.
join_covariates_df <- function(df,
                               join_covariates) {
  if (join_covariates) {
    assert_columns(df, "iso3", "year")
    df <- dplyr::left_join(df, augury::covariates_df, by = c("iso3", "year"))
  }
  df
}

#' Helper for `expand_df_filter()` to calculate min for keeping data
#'
#' Want to make sure that this returns `-Inf` rather than `Inf` for the `expand_df_filter()`
#' filter to work
#'
#' @param x Vector
#'
#' @return Value.
expand_df_min <- function(x) {
  ret <- min(which(is.na(x)), Inf)
  if (is.infinite(ret)) {
    ret <- -Inf
  }
  ret
}
