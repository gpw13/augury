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