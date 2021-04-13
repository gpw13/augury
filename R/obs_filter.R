#' Parse obs filter intro string to be evaluated
#'
#' This function parses a `obs_filter` argument passed through a `predict_...`
#' function into a string ready to be evaluated in `merge_prediction()`. The output
#' string will be parsed within a `case_when`. The `obs_filter` passed sets a
#' threshold for fitting models and generating predictions for data based on
#' number of observed data points. So, `'>=5'` would be that we only want to fit a model,
#' generate predictions and use them them to replace observations
#' if there are 5 or more observations. Thus, in `case_when()`, we pass the first
#' logical argument to be anywhere there is NOT `'>=5'` observations.
#'
#' @param obs_filter Obs filter string to assert
#' @param response Name of response column
#'
#' @return String for eval and parsing
parse_obs_filter <- function(obs_filter, response) {
  assert_obs_filter(obs_filter)
  if (is.null(obs_filter)) {
    obs_filter <- "FALSE"
  } else {
    obs_filter <- sprintf("!(sum(!is.na(.data[['%s']])) %s)", response, obs_filter)
  }
  obs_filter
}

#' @noRd
assert_obs_filter <- function(obs_filter) {
  assert_string(obs_filter, 1)
  if (!is.null(obs_filter)) {
    chk <- stringr::str_detect(obs_filter, "(^)(>|>=|<|<=|==|!=)([\\s]{0,1}[0-9]+$)")
    if (!chk) {
      stop("`obs_filter` must be NULL or a string with a logical operator and then a number, e.g. '>= 3'.",
           call. = FALSE)
    }
  }
}
