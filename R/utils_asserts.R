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

#' Assert that `x` is a character vector of length 1
#'
#' @param x Supposed string to test
assert_string_l1 <- function(x) {
  if (!is.null(x)) {
    lx <- length(x)
    if (!(is.character(x) & (lx == 1))) {
      stop(sprintf("`%s` must be a character vector of length 1, not %s of length %d.",
                   deparse(substitute(x)),
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
