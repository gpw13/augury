#' Remove groups from data frame if grouped
#'
#' Groups from input data frames into `predict_...` functions are removed to prevent
#' errors when fitting data and bringing predictions to the data frame.
#'
#' @param df Data frame.
remove_groups <- function(df) {
  chck <- dplyr::is_grouped_df(df)
  if (chck) {
    message(sprintf("`df` was grouped by column(s) %s but these have been removed.",
                    paste(dplyr::group_vars(df), collapse = ", ")))
    df <- dplyr::ungroup(df)
  }
  df
}
