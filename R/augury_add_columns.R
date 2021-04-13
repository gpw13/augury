#' Adds empty columns to df
#'
#' Used when models are not generated based on `obs_filter`.
#'
#' @param df Data frame
#' @param cols Vector of column names to add to df
augury_add_columns <- function(df, cols) {
  x <- rep(NA_real_, length(cols))
  names(x) <- cols
  tibble::add_column(df, !!!x[setdiff(names(x), names(df))])
}
