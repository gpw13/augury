
#' Minimizes dataset to data needed for modelling
#'
#' `get_model_data()` ensures that only variables necessary for the model
#' are included in the dataset and missing data and test sets are removed, if
#' `test_col` is not `NULL`. If `filter_na` is `"all"` (the default), then any
#' observations with `NA` values are removed using `na.omit()`. If `filter_na` is
#' `"response"` or `"predictors"` then only rows with missing dependent or independent
#' variables are removed, respectively. If `"none"`, then no filtering is done at all.
#'
#' @param formula_vars Character vector of variables used in the model. Can be
#'     extracted from a formula using `all.vars(fmla)`.
#' @inheritParams grouped_predict_general_mdl
#' @param reduce_columns Logical on whether or not to reduce the number of columns
#'     in the data to just those necessary for modelling.
#' @return A data frame.
get_model_data <- function(df,
                           formula_vars,
                           test_col,
                           group_col = NULL,
                           filter_na,
                           reduce_columns = TRUE) {

  # first make dependent variable NA for test cases, if applicable
  if (!is.null(test_col)) {
    assert_test_col(df[[test_col]])
    df[[formula_vars[1]]][df[[test_col]]] <- NA
  }

  # keep only data used in model and grouping column if applicable
  if (reduce_columns) {
    df <- df[,c(group_col, formula_vars)]
  }


  # filter data
  df <- filter_model_data(df, formula_vars, filter_na)

  df
}

#' Filters data for modeling
#'
#' Based on the `filter_na` argument, data is filtered for modelling based on
#' `NA` values within `df`.
#'
#' @inherit get_model_data params return
filter_model_data <- function(df,
                              formula_vars,
                              filter_na) {
  if (filter_na == "all") {
    df <- df[!is.na(df[[formula_vars]])]
  } else if (filter_na == "response") {
    df <- df[!is.na(df[[formula_vars[1]]]),]
  } else if (filter_na == "predictors") {
    fltr <- stats::complete.cases(df[,formula_vars[2:length(formula_vars)]])
    df <- df[fltr,]
  }
  df
}
