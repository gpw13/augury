#' Asserts formula and extract variables
#'
#' @inheritParams predict_general_mdl
parse_formula <- function(formula) {
  assert_formula(formula)
  all.vars(formula)
}


#' @noRd
assert_formula <- function(x) {
  if (!inherits(x, "formula")) {
    stop(sprintf("`formula` must be a formula object, not a %s.",
                 class(x)),
         call. = FALSE)
  }
}
