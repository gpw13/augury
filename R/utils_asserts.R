#' Assert that args in ellipses are columns in `df`
#'
#' @param df A data frame.
#' @param ... Column names to test.
assert_columns <- function(df, ...) {
  columns <- c(...)
  bad_cols <- columns[!(columns %in% names(df))]
  if (length(bad_cols) > 0) {
    stop(sprintf("Column(s) %s are not in the data frame",
                 paste(bad_cols, collapse = ", ")),
         call. = FALSE)
  }
}

#' Assert that test_col is of logical type
#'
#' @inheritParams predict_general_mdl
assert_test_col <- function(df, test_col) {
  if (!is.null(test_col)) {
    class_x <- class(df[[test_col]])
    if (class_x != "logical") {
      stop(sprintf("If `test_col` is not `NULL`, then it must be a logical vector, not a %s vector.",
                   class_x),
           call. = FALSE)
    }
  }
}

#' Assert that `df` is a data frame
#'
#' @param df Supposed data frame
assert_df <- function(df) {
  if (!is.data.frame(df)) {
    stop(sprintf("`df` must be a data frame, not a %s.",
                 class(df)[1]),
         call. = FALSE)
  }
}

#' Assert that `x` is a function
#'
#' @param x Model function to test
assert_model <- function(x) {
  if (!is.function(x)) {
    stop(sprintf("`model` must be an R function, not a %s.",
                 class(x)[1]),
         call. = FALSE)
  }
}

#' Assert that `x` is a character vector of length `n`
#'
#' @param x Supposed string to test
#' @param n Required length of vector
assert_string <- function(x, n) {
  if (!is.null(x)) {
    lx <- length(x)
    if (!(is.character(x) & (lx == n))) {
      stop(sprintf("`%s` must be a character vector of length %d, not %s of length %d.",
                   deparse(substitute(x)),
                   n,
                   class(x),
                   lx),
           call. = FALSE)
    }
  }
}

#' Assert that `x` is a function
#'
#' @param x Supposed function to test
assert_function <- function(x) {
  if (!is.function(x)) {
    stop(sprintf("`%s` must be a function, not a %s.",
                 deparse(substitute(x)),
                 class(x)[1]),
         call. = FALSE)
  }
}

#' Assert that h, for forecasting, is > 0
#'
#' @param h Number of missing values in series passed to forecast
assert_h <- function(h) {
  if (h == 0) {
    stop("No missing values to forecast supplied.",
         call. = FALSE)
  }
}

#' Assert numeric value
#'
#' @param x Numeric value to check
#' @param n Required length of vector
assert_numeric <- function(x, n) {
  if (!is.null(x)) {
    lx <- length(x)
    if (!(is.numeric(x) & (lx == n))) {
      stop(sprintf("`%s` must be a numeric vector of length %d, not %s of length %d.",
                   deparse(substitute(x)),
                   n,
                   class(x),
                   lx),
           call. = FALSE)
    }
  }
}

#' Assert columns in `df` are numeric, for use with average trend functions
#'
#' Checks that columns are numeric in `df`, and returns error message specifying that
#' they must either be numeric or explicitly included in the `average_cols` grouping
#' to be used for averaging.
#'
#' @param cols Columns to check
#' @param df Data frame with columns
assert_numeric_cols_avg <- function(cols, df) {
  nms <- sapply(df[,cols], is.numeric)
  if (!all(nms)) {
    stop(sprintf("%s must be numeric columns for use in averaging, or included in `average_cols` for grouping.",
                 paste(cols[!nms], collapse = ", ")),
         call. = FALSE)
  }
}


#' Assert columns in `df` are numeric
#'
#' @inheritParams assert_numeric_cols_avg
assert_numeric_cols <- function(cols, df) {
  nms <- sapply(df[,cols], is.numeric)
  if (!all(nms)) {
    stop(sprintf("%s must be numeric columns.",
                 paste(cols[!nms], collapse = ", ")),
         call. = FALSE)
  }
}

#' If using `error_correct`, then check that the columns are either in the
#' `formula_vars` or `average_cols`, otherwise produce an error.
#'
#' @inheritParams predict_inla_avg_trend
#' @inheritParams fit_general_model
assert_error_correct_avg_trend <- function(formula_vars,
                                           average_cols,
                                           error_correct,
                                           error_correct_cols) {
  if (error_correct) {
    if (!(error_correct_cols %in% c(formula_vars, average_cols))) {
      stop("`error_correct_cols` must be in either `formula` or `average_cols` for use with average trend functions.",
           call. = FALSE)
    }
  }
}

#' Assert sort column for use in average trend functions
#'
#' Takes in formula variables and average columns, as well as sort column argument.
#' If `sort_col` is provided, an error is generated if it is not
#' in `formula_vars` or `average_cols`. If `sort_col` is not provided, then
#' warnings are generated that no average trend can be generated.
#'
#' @inheritParams predict_inla_avg_trend
#' @inheritParams fit_general_model
#'
#' @return Column name for sorting averaged data to generate trend.
assert_group_sort_col <- function(formula_vars,
                                  average_cols,
                                  sort_col) {
  if (is.null(sort_col)) {
    warning("`sort_col` is NULL, so no average trend will be generated.",
            call. = FALSE)
  } else if (!(sort_col %in% c(formula_vars, average_cols))) {
    warning("`sort_col` not in `formula` or `average_cols`, so no average trend will be generated.",
            call. = FALSE)
  }
}
