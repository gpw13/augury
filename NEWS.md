# augury 0.3.3

* All `predict_...()` functions now treat confidence bounds in the same way as
    response and predicted values. Upper and lower bounds are generated in full
    in `pred_upper_col` and `pred_lower_col` and then the existing bounds replaced
    based on `replace_obs` and presence of non-missing values.
    
# augury 0.3.2

* Use `obs_filter` in all `predict_...` functions to replace `replace_filter`, allowing
    not just filtering of when to replace observations, but also not fitting models
    when not being used to improve speed and reduce errors if insufficient data
    for certain types of modeling.
* Add `expand_df()` function to allow easy generation of data frames with explicit
    missing values prior to passing to `predict_...` functions.

# augury 0.3.1

* Add back extrapolation (flat) to `predict_simple()`.
* Add `predict_aarr()` to allow the use of AARR for forecasting prevalence data.
* Implement `replace_filter` in all `predict_...` functions that allows for select
    use of predicted data based on number of observations so that different models
    can be used for different data typologies.
* Change defaults for `group_col` and `sort_col` to `"iso3"` and `"year"` respectively,
    since they are by far the most common usage.

# augury 0.3.0

* `predict_..._avg_trend()` functions implemented to allow the fitting of models
   by group and application of that trend to base data.
* Added in R-squared and root mean change error metrics to `model_error`.
* Weighted averaging option added to `predict_average()`.

# augury 0.2.0

* Added in mean absolute scaled error, median absolute error, and confidence
    bounds assessment to error metrics.
* Refitted all functions to perform grouped modeling with the `group_models`
    argument, removing the `grouped_predict_...` function aliases.
* Fix general functionality to support model building and testing.
* Add in `scale` and `probit` arguments to `predict_...` functions to enable
    automatic scaling and transforming of response variables prior to model
    fitting.

# augury 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Incorporated forecasting methods from the forecast package.
