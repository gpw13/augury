#' Bayesian analysis of structured additive models
#'
#' Provides a convenient wrapper around the inla function from the INLA package,
#' used to perform Bayesian analysis of additive models using Integrated Nested
#' Laplace approximation. Defaults set for what is used in Billions modeling,
#' while allowing all arguments to be adjusted for exploration and testing.
#'
#' See [INLA::inla()] for full documentation and guidance.
#'
#' @param formula A `inla` formula like
#'     `y ~ 1 + z + f(ind, model = "iid") + f(ind2, weights, model = "ar1")`.
#'     This is much like the formula for a `glm` except that smooth or spatial
#'     terms can be added to the right hand side of the formula. See [INLA::f]
#'     for full details.
#' @param data A data frame containing the variables in the model.
#' @param family A string indicating the likelihood family. The default is `gaussian` with
#'    identity link. All valid families available through `model_families()`. Family
#'    documentation available using [INLA::inla.doc()].
#' @param control.predictor See [INLA::control.predictor()]
#' @param control.inla See [INLA::control.inla()]
#' @param ... Additional arguments passed to [INLA::inla()].
#'
#' @return `inla_model` returns an object of class `"inla"`, a list containing
#'    arguments defined at [INLA::inla()].
#'
#' @export
inla_model <- function(formula,
                       data,
                       family = "gaussian",
                       control.predictor = list(compute = TRUE),
                       control.inla = list(strategy = "laplace"),
                       ...) {
  INLA::inla(formula = formula,
             data = data,
             family = family,
             control.predictor = control.predictor,
             control.inla = control.inla,
             ...)
}

#' Retrieve all valid inla likelihood families
#'
#' `likelihood_families()` gives a character vector of all valid likelihood families
#' available in [INLA::inla()].
#'
#' @return A character vector.
#'
#' @export
likelihood_families <- function() {
  names(INLA::inla.models()$likelihood)
}





perform_forecast <- function(inds){

  code       <- codebook %>% filter(ind == inds) %>% pull(code)
  lab.out    <- codebook %>% filter(ind == inds) %>% pull(lab)
  scl        <- codebook %>% filter(ind == inds) %>% pull(scl)
  modl       <- codebook %>% filter(ind == inds) %>% pull(modl)

  #################################################################################################################
  # print the code and filter
  print(code)
  datas.c    <- data.in.all %>% filter(gho_code == code)  %>% arrange(iso3, year)
  #################################################################################################################

  #################################################################################################################
  # observed data
  isos         <- datas.c %>% pull(iso3) %>% unique()
  obs.out      <- datas.c %>% select(iso3, val, year) %>%
    right_join(covariates.df %>% dplyr::filter(iso3 %in% isos) %>%
                 select(location_name, iso3, region) %>% distinct(), by = "iso3") %>%
    group_by(location_name, iso3, region, year) %>%
    summarise(val = mean(val, na.rm=T), .groups = 'drop_last') %>%
    ungroup() %>% mutate(pred = NA, lwr = NA, uppr = NA, source = "Obs") %>%
    arrange(iso3, year)

  #################################################################################################################
  # Input data for modeling
  dfin         <- datas.c %>%
    arrange(iso3, year) %>%
    group_by(iso3, year) %>%
    filter(!is.na(val)) %>%
    summarise(val = mean(val, na.rm = T), .groups = "drop") %>%
    ungroup() %>%
    group_by(iso3) %>% mutate(count = n()) %>% ungroup() %>%
    mutate(y = probit.tf(val/scl))

  #################################################################################################################
  # Infilling using linear interpolation in probit.tf space
  # Limited to greater than 2 observations

  dfg    = dfin %>% filter(count > 1) %>%
    select(iso3, year, y)

  if (nrow(dfg) > 1){
    isg    = sort(unique(dfg$iso3))
    nsg    = length(isg)
    dfflc  = list(dim = nsg)

    for (ig in 1:nsg){
      dfgo <- dfg %>% filter(iso3 == isg[ig]) %>% arrange(year)

      years    = dfgo$year
      vals     = dfgo$y
      nyo      = length(years)
      lc       = rep(NA, nyo - 1)

      yearsp   = years[1]:years[nyo]
      nyp      = length(yearsp)
      valsp    = rep(NA, nyp)
      valsp[1] = vals[1]

      u = 1; gapy = 0
      for (j in 1:(nyo - 1)){
        u = u + gapy
        valsp[u] = vals[j]
        gapy = years[j + 1] - years[j]
        lc[j]   = 1/gapy*(vals[j + 1] - vals[j])
        for (k in 1:(gapy-1)){
          valsp[u + k] = valsp[u] + k * lc[j]
        }
      }
      u        = u + gapy
      valsp[u] = vals[nyo]

      outl  <- data.table(iso3 = isg[ig], year = yearsp, y = valsp)
      dfflc[[ig]] <- outl
    }

    dfg    = rbindlist(dfflc)
  }

  dm <- dfin %>%
    filter(!(iso3 %in% unique(dfg$iso3))) %>%
    select(iso3, year, y) %>% rbind(dfg) %>%
    right_join(cov %>% select(location_name, iso3, year, region, sdi, e0), by = c("iso3","year")) %>%
    arrange(iso3, year)

  #################################################################################################################
  # Malaria is only for 40 endemic countries
  if (code == "MALARIA_ITNPOP"){
    maliso <- datas.c %>% filter(gho_code =="MALARIA_ITNPOP" & !is.na(val)) %>% pull(iso3) %>% unique()
    dm     <- dm %>% filter(iso3 %in% maliso)
  }

  #################################################################################################################
  # Input data n > 2: Use the time-series forecast with RW(2)
  # Possible option to go RW(1) but thats basicaly linear extrapolation

  iso3g2 <- dm %>%
    filter(!is.na(y)) %>% group_by(iso3) %>% mutate(count = n()) %>% ungroup() %>%
    filter(count > 2) %>% pull(iso3) %>% unique()

  dfg1 <- dm %>% filter(iso3 %in% iso3g2)
  if (nrow(dfg1) > 0){
    gpr.mods   <- dfg1 %>%
      arrange(iso3, year) %>% time.series(., modl) %>%
      mutate(val = NA, source = "RW(2)")
  }

  #################################################################################################################
  # Input data n <= 2: Use the covariates and forecast assuming RW(2)

  dfg2 <- dm %>% filter(!(iso3 %in% iso3g2))
  if (nrow(dfg2) > 0){
    gpr.mods.2 <- dm %>% arrange(iso3, year) %>% mixed.effects(., dfg2, modl) %>%
      mutate(val = NA, source = "Mixed-effects")
  }

  #################################################################################################################

  if (nrow(dfg1) > 0 & nrow(dfg2) > 0){
    dfout.all  <- rbind(gpr.mods, gpr.mods.2)
  } else if (nrow(dfg1) > 0 & nrow(dfg2) == 0) {
    dfout.all  <- gpr.mods
  } else {
    dfout.all  <- gpr.mods.2
  }

  dfin.out <- dfout.all %>%
    mutate(pred = pred*scl, lwr = lwr*scl, uppr = uppr*scl) %>% rbind(obs.out) %>%
    rbind(ihme_sdgs %>% filter(gho_code == code & iso3 %in% unique(dfout.all$iso3)) %>%
            select(names(dfout.all)))

  list(datas.c = datas.c, code = code, lab = lab.out, dfout = dfin.out, scl = scl)
}

#' Use Bayesian analysis of additive models to infill and project data
#'
#' `predict_inla()` uses a Integrated Nested Laplace approximation to fit a model
#' and use that model to infill and project the dependent variable. The function
#' uses [INLA::inla()] to perform the model fitting and prediction, and full
#' details and explanation of arguments that it can accept is available on that page.
#'
#' The function also allows for inputting of data type and source information
#' directly into the data frame if the `type_col` and `source_col` are specified
#' respectively.
#'
#' @inherit predict_general_mdl params return
#' @param ... Additional arguments passed to [INLA::inla()].
#' @param filter_na Character value specifying how, if at all, to filter `NA`
#'     values from the dataset prior to applying the model. By default, only
#'     observations with missing predictors are removed, although it can also remove
#'     rows only if they have missing dependent or independent variables, or no
#'     filtering at all. Model prediction and fitting are done in one pass with
#'     [INLA::inla()], so there will be no predictions if observations with
#'     missing dependent variables are removed.

#'
#' @return Depending on the value passed to `ret`, either a data frame with
#'     predicted data, a vector of errors, a fitted model, or a list with all 3.
#'
#' @export
predict_inla <- function(df,
                         formula,
                         ...,
                         ret = c("df", "all", "error", "model"),
                         test_col = NULL,
                         pred_col = "pred",
                         upper_col = "upper",
                         lower_col = "lower",
                         filter_na = c("predictors", "response", "all", "none"),
                         type_col = NULL,
                         types = c("imputed", "imputed", "projected"),
                         type_group = "iso3",
                         type_sort = "year",
                         source_col = NULL,
                         source = NULL,
                         replace_obs = c("missing", "all", "none"),
                         error_correct = FALSE,
                         error_correct_cols = NULL) {
  # Assertions and error checking
  assert_df(df)
  assert_model(model)
  formula_vars <- parse_formula(formula)
  assert_columns(df, formula_vars, test_col, type_col, source_col, error_correct_col)
  ret <- rlang::arg_match(ret)
  assert_test_col(df, test_col)
  assert_string_l1(pred_col)
  assert_string_l1(upper_col)
  assert_string_l1(lower_col)
  filter_na <- rlang::arg_match(filter_na)
  assert_string_l1(type_col)
  types <- rlang::arg_match(types)
  if (!is.null(type_col)) {
    assert_columns(df, type_group, type_sort)
  }
  assert_string_l1(source)
  replace_obs <- rlang::arg_match(replace_obs)

  # Filter data for modeling
  data <- get_model_data(df = df,
                         formula_vars = formula_vars,
                         test_col = test_col,
                         filter_na = filter_na)

  # Build model
  mdl <- INLA::inla(formula = formula,
                    data = data,
                    ...)

  if (ret == "model") {
    return(mdl)
  }

  # Get model predictions
  df <- predict_inla_data(data,
                          mdl,
                          pred_col,
                          upper_col,
                          lower_col)

  # Get error if being returned
  if (ret %in% c("all", "error")) {
    err <- model_error(df,
                       formula_vars[1],
                       pred_col,
                       test_col)

    if (ret == "error") {
      return(err)
    }
  }

  # Merge predictions into observations
  df <- merge_prediction(df,
                         formula_vars[1],
                         pred_col,
                         upper_col,
                         lower_col,
                         type_col,
                         types,
                         type_group,
                         type_sort,
                         source_col,
                         source,
                         replace_obs,
                         error_correct,
                         error_correct_cols)

  if (ret == "df") {
    return(df)
  } else if (ret == "all") {
    list(df = df,
         error = err,
         model = mdl)
  }
}

#' Generate prediction from an INLA output object
#'
#' `predict_inla_data()` generates a prediction vector from an [INLA::inla()]
#' output object, putting this prediction back into the data frame.
#'
#' @return A data frame.
predict_inla_data <- function(df,
                              model,
                              pred_col,
                              upper_col,
                              lower_col) {
  fitm        <- result$summary.fitted.values$mean
  fitl        <- result$summary.fitted.values$"0.025quant"
  fitu        <- result$summary.fitted.values$"0.975quant"
  fit <- model[["summary.fitted.values"]]
  df[[pred_col]] <- fit[["mean"]]
  df[[upper_col]] <- fit[["0.025quant"]]
  df[[lower_col]] <- fit[["0.975quant"]]
  df
}

#' Use Bayesian analysis of additive models to infill and project data by group
#'
#' `grouped_predict_inla()` uses a Integrated Nested Laplace approximation to fit a model
#' and use that model to infill and project the dependent variable by group. It
#' uses [INLA::inla()] to perform the model fitting and prediction, and full
#' details and explanation of arguments that it can accept is available on that page.
#'
#' The function also allows for inputting of data type and source information
#' directly into the data frame if the `type_col` and `source_col` are specified
#' respectively.
#'
#' @inherit predict_inla params
#' @inherit predict_general_mdl return
#'
#' @param group_col Column name to split data frame prior to model application.
#' @param ret Character vector specifying what values the function returns. Defaults
#'     to returning a data frame, but can return a vector of model error or a list
#'     with the data frame and error together.
#'
#' @export
grouped_predict_inla <- function(df,
                                 group_col = "iso3",
                                 formula,
                                 ...,
                                 ret = c("df", "all", "error", "model"),
                                 test_col = NULL,
                                 pred_col = "pred",
                                 upper_col = "upper",
                                 lower_col = "lower",
                                 filter_na = c("predictors", "response", "all", "none"),
                                 type_col = NULL,
                                 types = c("imputed", "imputed", "projected"),
                                 type_group = "iso3",
                                 type_sort = "year",
                                 source_col = NULL,
                                 source = NULL,
                                 replace_obs = c("missing", "all", "none"),
                                 error_correct = FALSE,
                                 error_correct_cols = NULL) {
  # Assertions and error checking
  assert_df(df)
  assert_model(model)
  formula_vars <- parse_formula(formula)
  assert_columns(df, formula_vars, test_col, type_col, source_col, error_correct_col)
  ret <- rlang::arg_match(ret)
  assert_test_col(df, test_col)
  assert_string_l1(pred_col)
  assert_string_l1(upper_col)
  assert_string_l1(lower_col)
  filter_na <- rlang::arg_match(filter_na)
  assert_string_l1(type_col)
  types <- rlang::arg_match(types)
  if (!is.null(type_col)) {
    assert_columns(df, type_group, type_sort)
  }
  assert_string_l1(source)
  replace_obs <- rlang::arg_match(replace_obs)

  # Filter data for modeling
  data <- get_model_data(df = df,
                         formula_vars = formula_vars,
                         test_col = test_col,
                         filter_na = filter_na)

  # Split data frames
  data <- dplyr::group_by(data, .data[[group_col]]) %>%
    dplyr::group_split()

  # Map modeling behavior
  df <- purrr::map_dfr(data,
                       function(x) {
                         mdl <- INLA::inla(formula = formula,
                                           data = x,
                                           ...)
                         predict_inla_data(x,
                                           mdl,
                                           pred_col,
                                           upper_col,
                                           lower_col)
                       })

  # Get error if being returned
  if (ret %in% c("all", "error")) {
    err <- model_error(df,
                       formula_vars[1],
                       pred_col,
                       test_col)

    if (ret == "error") {
      return(err)
    }
  }

  # Merge predictions into observations
  df <- merge_prediction(df,
                         formula_vars[1],
                         pred_col,
                         upper_col,
                         lower_col,
                         type_col,
                         types,
                         type_group,
                         type_sort,
                         source_col,
                         source,
                         replace_obs,
                         error_correct,
                         error_correct_cols)

  if (ret == "df") {
    return(df)
  } else if (ret == "all") {
    list(df = df,
         error = err,
         model = mdl)
  }
}
