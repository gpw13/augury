#' Parse replace filter intro string to be evaluated
#'
#' This function parses a `replace_filter` argument passed through a `predict_...`
#' function into a string ready to be evaluated in `merge_prediction()`. The output
#' string will be parsed within a `case_when`. The `replace_filter` passed sets a
#' threshold for replacing data based on number of observed data points. So,
#' `'>=5'` would be that we only want to use predictions and put them onto observations
#' if there are 5 or more observations. Thus, in `case_when()`, we pass the first
#' logical argument to be anywhere there is NOT `'>=5'` observations.
#'
#' @param replace_filter Replace filter string to assert
#' @param response Name of response column
#'
#' @return String for eval and parsing
parse_replace_filter <- function(replace_filter, response) {
  assert_replace_filter(replace_filter)
  if (is.null(replace_filter)) {
    replace_filter <- "FALSE"
  } else {
    replace_filter <- sprintf("!(sum(is.na(.data[['%s']])) %s)", response, replace_filter)
  }
  replace_filter
}

assert_replace_filter <- function(replace_filter) {
  assert_string(replace_filter, 1)
  if (!is.null(replace_filter)) {
    chk <- stringr::str_detect(replace_filter, "(^)(>|>=|<|<=|==|!=)([\\s]{0,1}[0-9]+$)")
    if (!chk) {
      stop("`replace_filter` must be NULL or a string with a logical operator and then a number, e.g. '>= 3'.",
           call. = FALSE)
    }
  }
}

