
<!-- README.md is generated from README.Rmd. Please edit that file -->

# augury <a href='https://github.com/caldwellst/augury'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/caldwellst/augury/workflows/R-CMD-check/badge.svg)](https://github.com/caldwellst/augury/actions)
<!-- badges: end -->

The goal of augury is to streamline the process of fitting models to
infill and forecast data, particularly for models used within the WHO’s
Triple Billion framework.

## Installation

You can install augury from
[GitHub](https://github.com/caldwellst/augury). The package depends on
the [INLA package](https://www.r-inla.org/home) which is not available
on CRAN. You will need to separately install that prior to installing
augury. The below code should allow you to install the packages without
any trouble:

``` r
if (!require("INLA")) install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
remotes::install_github("caldwellst/augury")
```

If you are reinstalling the package to update something, you should make
sure you have necessary dependencies, but to avoid re-downloading the
large INLA tarball, you can use:

``` r
remotes::install_github("caldwellst/augury", dependencies = FALSE)
```

## Overview

Most of the functions available through the package are designed to
streamline fitting a model and replacing missing observations within a
single data frame in one pass.

-   `predict_general_mdl()` is a function that takes in a data frame,
    generic R modelling function (such as `stats::lm`), and fits a model
    and returns newly projected data, error metrics, the fitted model
    itself, or a list of all 3.
-   `predict_inla()` is a function similar to the above, except rather
    than taking a generic modelling function it accepts a formula and
    additional argument passed to `INLA::inla()` to perform Bayesian
    analysis of structured additive models.
-   `predict_forecast()` instead uses forecasting methods available from
    `forecast::forecast()` to perform exponential smoothing or other
    time series models on a response variable.

Given that models might need to be fit separately to different portions
of a data frame, such as fitting time series models for each country
individually, these functions now accept the argument `group_models`
that determines whether to fit separate models to each group in the data
frame before joining them back together into the original data frame.

## Additional model wrappers

Additional functions are provided as requested to streamlining fitting
of specific models. For general modeling, there are grouped and
individual functions for `stats::lm()`, `stats::glm()`, and
`lme4::lmer()` to fit linear models, generalized linear models, and
linear mixed-effects models respectively. These wrappers are:

-   `predict_lm()`
-   `predict_glm()`
-   `predict_lmer()`

As well, wrappers are provided around INLA for models currently in place
for the Triple Billion framework. These are a time series model with no
additional covariates (fit individually by country) and a mixed-effects
model using covariates.

-   `predict_inla_me()`
-   `predict_inla_ts()`

And for forecasting, generic functions are provided for simple
exponential smoothing and Holt’s linear trend exponential smoothing.

-   `predict_holt()`
-   `predict_ses()`

## Covariates

For convenience, the covariates used within the default INLA modeling
are exported from the package and can be easily joined up to a data
frame for use in modeling. These are available through
`augury::covariates_df`.

## Grouped trend development

While we typically want to fit a model to a data set directly, augury
has a set of `predict_..._avg_trend()` functions that allows you to
average data by group (e.g. by region), fit the model to the grouped and
averaged data, and extract that trend to apply to the original dataset.
See the section below on **Average trend modeling** for more details.

## Error metrics

When doing model selection, it is always important to use metrics to
evaluate model fit and predictive accuracy. All `predict_...` functions
in augury use the same evaluation framework, implemented through
`model_error()`. If a `test` column is defined, then the evaluation
framework is only applied to this set

## Additional functionality

To help streamline other portions of the modeling process, some other
functions are included in the package.

-   `probit_transform()` uses a probit transformation on select columns
    of a data frame, and inverses it if specified.
-   `scale_transform()` scales a vector by a single number, very simple,
    and can inverse the scaling if specified.
-   `predict_simple()` performs linear interpolation or flat
    extrapolation in the same manner as the other `predict_...`
    functions, but without modeling or confidence bounds.
-   `predict_average()` performs averaging by groups of columns in the
    same manner as the other `predict_...` functions, but without
    modeling or confidence bounds.

Together, they can be used to transform and scale data for better
modeling, and then inverse these to get data back into the original
feature space.

## INLA modeling examples

In order to show the INLA modeling wrappers provided in augury, we will
look at two datasets publicly available on the World Health
Organization’s Global Health Observatory. These can be accessed using
the [ghost package](https://github.com/caldwellst/ghost), which provides
an R interface for the GHO OData API.

## Time series modeling

The first indicator will be on safe sanitation. We will also use the
[billionaiRe](https://github.com/caldwellst/billionaiRe) package to
quickly transform the GHO data into the simple format used by augury,
billionaiRe, and other packages.

``` r
library(augury)

df <- ghost::gho_data("WSH_SANITATION_SAFELY_MANAGED",
                      query = "$filter=Dim1 eq 'TOTL'") %>%
  billionaiRe::wrangle_gho_data(source = "WHO GHO",
                                type = "estimated")

head(df)
#> # A tibble: 6 x 9
#>   iso3   year ind             value lower upper source  type      other_detail
#>   <chr> <int> <chr>           <dbl> <lgl> <lgl> <chr>   <chr>     <lgl>       
#> 1 ALB    2000 hpop_sanitation  38.6 NA    NA    WHO GHO estimated NA          
#> 2 ALB    2001 hpop_sanitation  38.4 NA    NA    WHO GHO estimated NA          
#> 3 ALB    2002 hpop_sanitation  38.3 NA    NA    WHO GHO estimated NA          
#> 4 ALB    2003 hpop_sanitation  38.1 NA    NA    WHO GHO estimated NA          
#> 5 ALB    2004 hpop_sanitation  37.9 NA    NA    WHO GHO estimated NA          
#> 6 ALB    2005 hpop_sanitation  37.8 NA    NA    WHO GHO estimated NA
```

Now that we have the input data available from the GHO in an easy to use
format, we can now join up with the `covariates_df` available in augury
and run a time series model to predict sanitation out to 2023. For
simplicity, let’s just look at Albania, with ISO3 code `"ALB"`.

``` r
library(dplyr)

df <- left_join(covariates_df,
                df,
                by = c("iso3", "year")) %>%
  filter(iso3 == "ALB")

head(df)
#> # A tibble: 6 x 15
#>   iso3   year year_n region    sdi sdi_scaled    e0 e0_scaled ind    value lower
#>   <chr> <int>  <dbl> <chr>   <dbl>      <dbl> <dbl>     <dbl> <chr>  <dbl> <lgl>
#> 1 ALB    2000      1 Southe… 0.604      0.650  74.0     0.752 hpop_…  38.6 NA   
#> 2 ALB    2001      2 Southe… 0.61       0.657  74.3     0.759 hpop_…  38.4 NA   
#> 3 ALB    2002      3 Southe… 0.614      0.661  74.6     0.766 hpop_…  38.3 NA   
#> 4 ALB    2003      4 Southe… 0.619      0.667  74.8     0.771 hpop_…  38.1 NA   
#> 5 ALB    2004      5 Southe… 0.625      0.673  75.0     0.776 hpop_…  37.9 NA   
#> 6 ALB    2005      6 Southe… 0.632      0.681  75.2     0.780 hpop_…  37.8 NA   
#> # … with 4 more variables: upper <lgl>, source <chr>, type <chr>,
#> #   other_detail <lgl>
```

Of course, the only “covariate” being used in this time series model is
going to be `year_n`, but the rest are available if we want to expand to
test other types of modeling. Let’s run the modeling now. We are going
to scale the data and probit transform it before and after the modeling.
We will use the `predict_inla_ts()` to fit a time series model to the
data.

``` r
modeled_df <- df %>%
  scale_transform("value") %>%
  probit_transform("value") %>%
  predict_inla_ts(type_col = "type",
                  source_col = "source",
                  source = "augury modeling") %>%
  probit_transform(c("value", "pred", "upper", "lower"), inverse = TRUE) %>%
  scale_transform(c("value", "pred", "upper", "lower"), divide = FALSE)

# Only look at recent years and relevant columns

modeled_df %>%
  filter(year > 2015) %>%
  select(iso3, year, value, pred, lower, upper, source, type)
#> # A tibble: 10 x 8
#>    iso3   year value  pred lower upper source  type     
#>    <chr> <int> <dbl> <dbl> <dbl> <dbl> <chr>   <chr>    
#>  1 ALB    2016  39.8  39.9  39.6  40.1 WHO GHO estimated
#>  2 ALB    2017  39.9  39.9  39.6  40.1 WHO GHO estimated
#>  3 ALB    2018  39.9  39.9  39.2  40.6 <NA>    projected
#>  4 ALB    2019  39.9  39.9  38.7  41.1 <NA>    projected
#>  5 ALB    2020  39.9  39.9  38.1  41.7 <NA>    projected
#>  6 ALB    2021  39.9  39.9  37.4  42.4 <NA>    projected
#>  7 ALB    2022  39.9  39.9  36.7  43.2 <NA>    projected
#>  8 ALB    2023  39.9  39.9  35.8  44.0 <NA>    projected
#>  9 ALB    2024  39.9  39.9  35.0  45.0 <NA>    projected
#> 10 ALB    2025  39.9  39.9  34.0  46.0 <NA>    projected
```

And there we go, we have now fit a time series model to our data,
provided new type and source, and merged this into our existing data
frame. However, in this setup, the error calculations returned by
`predict_inla_ts()` are calculated in the probit space. If we wanted to
scale and probit transform the response variable prior to model fitting,
but still calculate error metrics and automatically return the response
and predicted values back in the original space, we can set
`scale = 100` and `probit = TRUE` within `predict_inla_ts()`.

``` r
df %>%
  predict_inla_ts(scale = 100,
                  probit = TRUE,
                  type_col = "type",
                  source_col = "source",
                  source = "augury modeling") %>%
  filter(year > 2015) %>%
  select(iso3, year, value, pred, lower, upper, source, type)
#> # A tibble: 10 x 8
#>    iso3   year value  pred lower upper source  type     
#>    <chr> <int> <dbl> <dbl> <dbl> <dbl> <chr>   <chr>    
#>  1 ALB    2016  39.8  39.9  39.6  40.1 WHO GHO estimated
#>  2 ALB    2017  39.9  39.9  39.6  40.1 WHO GHO estimated
#>  3 ALB    2018  39.9  39.9  39.2  40.6 <NA>    projected
#>  4 ALB    2019  39.9  39.9  38.7  41.1 <NA>    projected
#>  5 ALB    2020  39.9  39.9  38.1  41.7 <NA>    projected
#>  6 ALB    2021  39.9  39.9  37.4  42.4 <NA>    projected
#>  7 ALB    2022  39.9  39.9  36.7  43.2 <NA>    projected
#>  8 ALB    2023  39.9  39.9  35.8  44.0 <NA>    projected
#>  9 ALB    2024  39.9  39.9  35.0  45.0 <NA>    projected
#> 10 ALB    2025  39.9  39.9  34.0  46.0 <NA>    projected
```

And we can see that the results here are the same as manually scaling
and probit transforming the variables.

## Mixed-effects modeling

Now we will look at another indicator, a composite of 13 International
Health Regulations core capacity scores, SPAR version. Since countries
only have two data points at most, we will use mixed-effects modeling to
infill and project the data.

``` r
df <- ghost::gho_data("SDGIHR2018") %>%
  billionaiRe::wrangle_gho_data(source = "Electronic State Parties Self-Assessment Annual Reporting Tool (e-SPAR)",
                                type = "reported")

head(df)
#> # A tibble: 6 x 9
#>   iso3   year ind   value lower upper source                  type  other_detail
#>   <chr> <int> <chr> <dbl> <lgl> <lgl> <chr>                   <chr> <lgl>       
#> 1 AFG    2018 espar    35 NA    NA    Electronic State Parti… repo… NA          
#> 2 AFG    2019 espar    43 NA    NA    Electronic State Parti… repo… NA          
#> 3 AGO    2018 espar    59 NA    NA    Electronic State Parti… repo… NA          
#> 4 AGO    2019 espar    63 NA    NA    Electronic State Parti… repo… NA          
#> 5 ALB    2018 espar    NA NA    NA    Electronic State Parti… repo… NA          
#> 6 ALB    2019 espar    62 NA    NA    Electronic State Parti… repo… NA
```

With this, let’s go straight into the modeling like last time, except we
will now use `predict_inla_me()` for mixed-effects modeling using
covariates found in `covariates_df`. This time, we want to model a first
order auto-regressive process across time rather than a second-order
random walk, so we use the `"ar1"` model available in INLA.

``` r
modeled_df <- df %>%
  right_join(covariates_df, by = c("iso3", "year")) %>%
  scale_transform("value") %>%
  probit_transform("value") %>%
  predict_inla_me(model = "ar1",
                  type_col = "type",
                  source_col = "source",
                  source = "WHO DDI Preliminary infilling and projections") %>%
  probit_transform(c("value", "pred", "upper", "lower"), inverse = TRUE) %>%
  scale_transform(c("value", "pred", "upper", "lower"), divide = FALSE)

# Look at an example for Afghanistan

modeled_df %>%
  filter(year > 2017, iso3 == "AFG") %>%
  select(iso3, year, value, pred, lower, upper, source, type)
#> # A tibble: 8 x 8
#>   iso3   year value  pred lower upper source                              type  
#>   <chr> <int> <dbl> <dbl> <dbl> <dbl> <chr>                               <chr> 
#> 1 AFG    2018  35    38.4  30.7  46.6 Electronic State Parties Self-Asse… repor…
#> 2 AFG    2019  43    39.4  31.6  47.6 Electronic State Parties Self-Asse… repor…
#> 3 AFG    2020  40.3  40.3  32.5  48.5 <NA>                                proje…
#> 4 AFG    2021  41.3  41.3  33.4  49.4 <NA>                                proje…
#> 5 AFG    2022  42.1  42.1  34.3  50.2 <NA>                                proje…
#> 6 AFG    2023  43.1  43.1  35.2  51.1 <NA>                                proje…
#> 7 AFG    2024  43.9  43.9  36.0  52.0 <NA>                                proje…
#> 8 AFG    2025  44.8  44.8  36.9  52.8 <NA>                                proje…
```

And exactly as we were able to do with the time series modeling, we now
have infilled missing data for this indicator using mixed-effects
modeling in INLA.

## Building further

Building further on this work, you can tweak any of the arguments passed
to these INLA models or use the base `predict_inla()` and other
covariates to test and compare other models. There is much more
functionality to test modeling accuracy and iteratively develop methods
available in this package not shown here, so please continue to explore
and play around.

## Forecasting examples

To look at using forecast methods to predict data, we will again be
using the [ghost package](https://github.com/caldwellst/ghost), which
provides an R interface for the GHO OData API and accessing data on
blood pressure. We will load in data for the USA and Great Britain
initially, which provide full time series from 1975 to 2015.

``` r
library(augury)

df <- ghost::gho_data("BP_04", query = "$filter=SpatialDim in ('USA', 'GBR') and Dim1 eq 'MLE' and Dim2 eq 'YEARS18-PLUS'") %>%
  billionaiRe::wrangle_gho_data() %>%
  dplyr::right_join(tidyr::expand_grid(iso3 = c("USA", "GBR"),
                                       year = 1975:2017))
#> Joining, by = c("iso3", "year")

head(df)
#> # A tibble: 6 x 9
#>   iso3   year ind   value lower upper source type  other_detail
#>   <chr> <int> <chr> <dbl> <dbl> <dbl> <lgl>  <chr> <lgl>       
#> 1 GBR    1975 bp     37.8  26.7  49.1 NA     <NA>  NA          
#> 2 GBR    1976 bp     37.6  27.4  48   NA     <NA>  NA          
#> 3 GBR    1977 bp     37.3  27.9  46.8 NA     <NA>  NA          
#> 4 GBR    1978 bp     37.1  28.4  45.9 NA     <NA>  NA          
#> 5 GBR    1979 bp     36.9  28.8  45.2 NA     <NA>  NA          
#> 6 GBR    1980 bp     36.7  29.2  44.4 NA     <NA>  NA
```

With this data, we can now use the `predict_forecast()` function like we
would any of the other `predict_...` functions from augury to forecast
out to 2017. First, we will do this just on USA data and use the
`forecast::holt` to forecast using exponential smoothing.

``` r
usa_df <- dplyr::filter(df, iso3 == "USA")

predict_forecast(usa_df,
                 forecast::holt,
                 "value",
                 sort_col = "year") %>%
  dplyr::filter(year >= 2012)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
#> # A tibble: 6 x 10
#>   iso3   year ind   value lower upper source type  other_detail  pred
#>   <chr> <int> <chr> <dbl> <dbl> <dbl> <lgl>  <chr> <lgl>        <dbl>
#> 1 USA    2012 bp     15.7  NA    NA   NA     <NA>  NA            NA  
#> 2 USA    2013 bp     15.5  NA    NA   NA     <NA>  NA            NA  
#> 3 USA    2014 bp     15.4  NA    NA   NA     <NA>  NA            NA  
#> 4 USA    2015 bp     15.3  NA    NA   NA     <NA>  NA            NA  
#> 5 USA    2016 <NA>   15.2  15.0  15.3 NA     <NA>  NA            15.2
#> 6 USA    2017 <NA>   15.1  14.9  15.2 NA     <NA>  NA            15.1
```

Of course, we might want to run these models all together for each
country individually. In this case, we can use the `group_models = TRUE`
function to perform the forecast individually by country. To save a bit
of limited time, let’s use the wrapper `predict_holt()` to automatically
supply `forecast::holt` as the forecasting function.

``` r
predict_holt(df,
             response = "value",
             group_col = "iso3",
             group_models = TRUE,
             sort_col = "year") %>%
  dplyr::filter(year >= 2014, year <= 2017)
#> # A tibble: 8 x 10
#>   iso3   year ind   value lower upper source type  other_detail  pred
#>   <chr> <int> <chr> <dbl> <dbl> <dbl> <lgl>  <chr> <lgl>        <dbl>
#> 1 GBR    2014 bp     18.5  NA    NA   NA     <NA>  NA            NA  
#> 2 GBR    2015 bp     17.9  NA    NA   NA     <NA>  NA            NA  
#> 3 GBR    2016 <NA>   17.3  17.2  17.4 NA     <NA>  NA            17.3
#> 4 GBR    2017 <NA>   16.7  16.5  16.9 NA     <NA>  NA            16.7
#> 5 USA    2014 bp     15.4  NA    NA   NA     <NA>  NA            NA  
#> 6 USA    2015 bp     15.3  NA    NA   NA     <NA>  NA            NA  
#> 7 USA    2016 <NA>   15.2  15.0  15.3 NA     <NA>  NA            15.2
#> 8 USA    2017 <NA>   15.1  14.9  15.2 NA     <NA>  NA            15.1
```

Et voila, we have the same results for the USA and have also ran
forecasting on Great Britain as well. However, you should be careful on
the data that is supplied for forecasting. The `forecast` package
functions default to using the longest, contiguous non-missing data for
forecasting. `augury` instead automatically pulls the latest contiguous
observed data to use for forecasting, to ensure that older data is not
prioritized over new data. However, this means any break in a time
series will prevent data before that from being used.

``` r
bad_df <- dplyr::tibble(x = c(1:4, NA, 3:2, rep(NA, 4)))

predict_holt(bad_df, "x", group_col = NULL, sort_col = NULL)
#> # A tibble: 11 x 4
#>         x   pred upper  lower
#>     <dbl>  <dbl> <dbl>  <dbl>
#>  1  1     NA     NA    NA    
#>  2  2     NA     NA    NA    
#>  3  3     NA     NA    NA    
#>  4  4     NA     NA    NA    
#>  5 NA     NA     NA    NA    
#>  6  3     NA     NA    NA    
#>  7  2     NA     NA    NA    
#>  8  1.17   1.17   2.55 -0.217
#>  9  0.338  0.338  2.33 -1.66 
#> 10 -0.494 -0.494  2.14 -3.12 
#> 11 -1.32  -1.32   1.98 -4.63
```

It’s advisable to consider if other data infilling or imputation methods
should be used to generate a full time series prior to the use of
forecasting methods to prevent issues like above from impacting the
predictive accuracy.

## Simple prediction methods

The simple methods available in augury are easy to use, but provide the
same functionality of allowing a test column and returning error metrics
as the more complex modeling functions available in the package. Let’s
use data on alcohol from the GHO to demonstrate the functionality.

``` r
library(augury)

df <- ghost::gho_data("SA_0000001688",
                      query = "$filter=Dim1 eq 'BTSX'") %>%
  billionaiRe::wrangle_gho_data(source = "WHO GHO",
                                type = "estimated") %>%
  dplyr::arrange(iso3, year)

head(df)
#> # A tibble: 6 x 9
#>   iso3   year ind     value lower upper source  type      other_detail
#>   <chr> <int> <chr>   <dbl> <lgl> <lgl> <chr>   <chr>     <lgl>       
#> 1 AFG    2010 alcohol 0.211 NA    NA    WHO GHO estimated NA          
#> 2 AFG    2015 alcohol 0.209 NA    NA    WHO GHO estimated NA          
#> 3 AFG    2018 alcohol 0.215 NA    NA    WHO GHO estimated NA          
#> 4 AGO    2000 alcohol 2.76  NA    NA    WHO GHO estimated NA          
#> 5 AGO    2005 alcohol 4.89  NA    NA    WHO GHO estimated NA          
#> 6 AGO    2010 alcohol 8.16  NA    NA    WHO GHO estimated NA
```

Here we can see that data has time series and gaps in years. We can use
linear interpolation and flat extrapolation here to get data out to
2023.

``` r
df <- tidyr::expand_grid(iso3 = unique(df$iso3),
                         year = 2000:2023) %>%
  dplyr::left_join(df, by = c("iso3", "year"))

df %>%
  dplyr::filter(iso3 == "AFG",
                year >= 2010,
                year <= 2018) %>%
  dplyr::select(iso3,
                year,
                value)
#> # A tibble: 9 x 3
#>   iso3   year  value
#>   <chr> <int>  <dbl>
#> 1 AFG    2010  0.211
#> 2 AFG    2011 NA    
#> 3 AFG    2012 NA    
#> 4 AFG    2013 NA    
#> 5 AFG    2014 NA    
#> 6 AFG    2015  0.209
#> 7 AFG    2016 NA    
#> 8 AFG    2017 NA    
#> 9 AFG    2018  0.215
```

Let’s now use our linear interpolation and flat extrapolation on this
data.

``` r
pred_df <- predict_simple(df,
                          group_col = "iso3",
                          sort_col = "year") 

pred_df %>%
  dplyr::filter(iso3 == "AFG",
                year >= 2010,
                year <= 2018) %>%
  dplyr::select(iso3, year, value)
#> # A tibble: 9 x 3
#>   iso3   year  value
#>   <chr> <int>  <dbl>
#> 1 AFG    2010  0.211
#> 2 AFG    2011 NA    
#> 3 AFG    2012 NA    
#> 4 AFG    2013 NA    
#> 5 AFG    2014 NA    
#> 6 AFG    2015  0.209
#> 7 AFG    2016 NA    
#> 8 AFG    2017 NA    
#> 9 AFG    2018  0.215
```

And we can see our linear interpolation there. We can also see the flat
extrapolation.

``` r
pred_df %>%
  dplyr::filter(iso3 == "AFG",
                year > 2016) %>%
  dplyr::select(iso3, year, value)
#> # A tibble: 7 x 3
#>   iso3   year  value
#>   <chr> <int>  <dbl>
#> 1 AFG    2017 NA    
#> 2 AFG    2018  0.215
#> 3 AFG    2019 NA    
#> 4 AFG    2020 NA    
#> 5 AFG    2021 NA    
#> 6 AFG    2022 NA    
#> 7 AFG    2023 NA
```

We can use the `predict_average()` function in much the same way, except
it is most useful when we have robust series for a set of countries, and
no data for others. We can then use something like the regional average
to infill data for missing countries.

``` r
df <- ghost::gho_data("SDGPOLLUTINGFUELS") %>%
  billionaiRe::wrangle_gho_data(source = "WHO GHO",
                                type = "estimated") %>%
  dplyr::filter(whoville::is_who_member(iso3))

x <- whoville::who_member_states()
x[!(x %in% df$iso3)]
#> [1] "LBN" "CUB" "BGR" "LBY"
```

Above, we have 4 missing WHO member states, Lebanon, Cuba, Bulgaria, and
Libya. Let’s use regional averaging to fill in this data. We can use the
most recent World Bank income groups from the [whoville
package](https://github.com/caldwellst/whoville) as our relevant group.

``` r
df <- tidyr::expand_grid(iso3 = x,
                         year = 2000:2018) %>%
  dplyr::left_join(df, by = c("iso3", "year")) %>%
  dplyr::mutate(region = whoville::iso3_to_regions(iso3, region = "wb_ig"))

predict_average(df,
                average_cols = c("region", "year"),
                group_col = "iso3",
                sort_col = "year",
                type_col = "type",
                source_col = "source",
                source = "WB IG regional averages") %>%
  dplyr::filter(iso3 == "LBN")
#> # A tibble: 19 x 11
#>    iso3   year ind   value lower upper source type    other_detail region  pred
#>    <chr> <int> <chr> <dbl> <lgl> <lgl> <chr>  <chr>   <lgl>        <chr>  <dbl>
#>  1 LBN    2000 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#>  2 LBN    2001 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#>  3 LBN    2002 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#>  4 LBN    2003 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#>  5 LBN    2004 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#>  6 LBN    2005 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#>  7 LBN    2006 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#>  8 LBN    2007 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#>  9 LBN    2008 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#> 10 LBN    2009 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#> 11 LBN    2010 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#> 12 LBN    2011 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#> 13 LBN    2012 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#> 14 LBN    2013 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#> 15 LBN    2014 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#> 16 LBN    2015 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#> 17 LBN    2016 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#> 18 LBN    2017 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
#> 19 LBN    2018 <NA>   65.9 NA    NA    <NA>   imputed NA           UMC     65.9
```

Hope these examples have been clear and highlight some of the usefulness
of these simple modelling functions.

## Average trend modeling

While we most often want to directly build models on our original
dataset to generate predicted values, we might instead want to generate
average trends across larger groups instead, and then apply this to our
original data. For instance, generating trends by region, and then
applying those regional trends back to the country level. The
`predict_...avg_trend()` functions in augury allow us to do just that,
applying any of the models we are used to a grouped set of columns.

These work across specific groups, specified by `average_cols`, and
averaging numeric values specified as the response variable or variables
extracted from a `formula`. The specified model is then fit to this
averaged data, and the predicted values are joined back up to the
original data frame. Let’s look at an example using blood pressure data,
which has a comprehensive time series.

``` r
library(augury)

df <- ghost::gho_data("BP_04", query = "$filter=Dim1 eq 'MLE' and Dim2 eq 'YEARS18-PLUS'") %>%
  billionaiRe::wrangle_gho_data() %>%
  dplyr::right_join(covariates_df) %>%
  dplyr::select(iso3, year, year_n, value) %>%
  dplyr::filter(whoville::is_who_member(iso3),                # keep WHO member states
                year >= 2000, year <= 2023) %>%               # get relevant years  
  dplyr::mutate(who_region = whoville::iso3_to_regions(iso3)) # get WHO regions
#> Joining, by = c("iso3", "year")

ur <- unique(df$who_region)
ur
#> [1] "EMR"  "AFR"  "EUR"  "AMR"  "WPR"  "SEAR"
```

Alright, so, here we have 6 WHO regions. We will use these regions to
fit a model to and use INLA to predict out to 2023, then apply these
trends to input countries.

``` r
pred_df <- df %>%
  predict_inla_avg_trend(formula = value ~ f(year_n, model = "rw2"),
                         average_cols = c("who_region", "year_n"),
                         group_models = TRUE,
                         group_col = "iso3",
                         sort_col = "year_n")

pred_df %>%
  dplyr::filter(iso3 == "AFG", year >= 2013)
#> # A tibble: 11 x 8
#>    iso3   year year_n value who_region  pred upper lower
#>    <chr> <int>  <dbl> <dbl> <chr>      <dbl> <dbl> <dbl>
#>  1 AFG    2013     14  30.4 EMR         29.4  29.4  29.4
#>  2 AFG    2014     15  30.4 EMR         29.4  29.4  29.4
#>  3 AFG    2015     16  30.4 EMR         29.4  29.4  29.4
#>  4 AFG    2016     17  29.4 EMR         29.4  29.4  29.4
#>  5 AFG    2017     18  29.4 EMR         29.4  29.4  29.4
#>  6 AFG    2018     19  29.4 EMR         29.4  29.4  29.4
#>  7 AFG    2019     20  29.4 EMR         29.4  29.4  29.4
#>  8 AFG    2020     21  29.4 EMR         29.4  29.4  29.4
#>  9 AFG    2021     22  29.4 EMR         29.4  29.4  29.4
#> 10 AFG    2022     23  29.4 EMR         29.4  29.4  29.4
#> 11 AFG    2023     24  29.4 EMR         29.4  29.4  29.4
```

Above, we can see we have a generated a model using 2nd order random
walk with INLA, however, the model was generated by averaging data to
WHO regions first, fitting the random walk to each reach (since
`group_models = TRUE`) and then fitting those trends to the original
data. Note some specifics of what had to be set, as the
`predict_..._avg_trend()` functions are slightly more complex than
others:

-   `average_cols` must contain the `sort_col`. So, since we use
    `year_n` in the time series rather than `year`, we will sort by that
    this time.
-   `average_cols` refers to the groupings used for averaging (we take
    the average for each WHO region and year in this case). Then, the
    model is fit to `average_cols` that are NOT the `sort_col`.
-   `group_col` is the groupings used on the *original data frame*,
    which is still necessary here when applying the trend back to the
    original data.
-   If a variable is in `formula`, it must either be in `average_cols`
    or it must be a numeric column that can be averaged. This is because
    the formula is applied to the data frame *after* `dplyr::group_by()`
    and `dplyr::summarize()` have reduced it.

To highlight this point, in the above example, what’s actually happening
is we are actually fitting the model on the summarized data:

``` r
df %>%
  dplyr::group_by(who_region, year_n) %>%
  dplyr::summarize(value = mean(value, na.rm = T)) %>%
  head()
#> `summarise()` has grouped output by 'who_region'. You can override using the `.groups` argument.
#> # A tibble: 6 x 3
#> # Groups:   who_region [1]
#>   who_region year_n value
#>   <chr>       <dbl> <dbl>
#> 1 AFR             1  29.7
#> 2 AFR             2  29.6
#> 3 AFR             3  29.5
#> 4 AFR             4  29.4
#> 5 AFR             5  29.3
#> 6 AFR             6  29.2
```

Since `average_cols = c("who_region", "year_n")`, we took the mean of
all values in `formula` not in `average_cols`, in this case just
`value`. If for instance, we tried to specify a model using `iso3` in
the `formula`:

``` r
predict_inla_avg_trend(df,
                       formula = value ~ iso3 + f(year_n, model = "rw2"),
                       average_cols = c("who_region", "year_n"),
                       group_models = TRUE,
                       group_col = "iso3",
                       sort_col = "year_n")
#> Error: iso3 must be numeric columns for use in averaging, or included in `average_cols` for grouping.
```

We get an error message indicating that iso3 must be numeric or included
in `average_cols` for grouping. This is because without it being numeric
or in the `average_cols`, there’s no way
`dplyr::group_by() %>% dplyr::summarize()` a non-numeric column
automatically (how would we reduce country-level ISO3 codes to the
regional level?).

While slightly complex, ensuring you follow the above means you should
easily and successfully get out meaningful trend predictions for your
data frames using trends generated on grouped data.
