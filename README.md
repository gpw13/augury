
<!-- README.md is generated from README.Rmd. Please edit that file -->

# augury

<!-- badges: start -->

[![R build
status](https://github.com/caldwellst/augury/workflows/R-CMD-check/badge.svg)](https://github.com/caldwellst/augury/actions)
[![Travis build
status](https://travis-ci.com/caldwellst/augury.svg?branch=master)](https://travis-ci.com/caldwellst/augury)
<!-- badges: end -->

The goal of augury is to streamline the process of fitting models to
infill and forecast data, particularly for models used within the WHO’s
Triple Billion framework

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
-   \`predict\_lmer()

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

## Additional functionality

To help streamline other portions of the modeling process, some other
functions are included in the package.

-   `probit_transform()` uses a probit transformation on select columns
    of a data frame, and inverses it if specified.
-   `scale_transform()` scales a vector by a single number, very simple,
    and can inverse the scaling if specified.
-   `predict_simple()` performs linear interpolation or flat
    extrapolation in the same manner as the other `predict_...`
    functions, but without modelling or confidence bounds.

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
#>   iso3   year year_n region   sdi sdi_scaled    e0 e0_scaled ind   value lower
#>   <chr> <dbl>  <dbl> <chr>  <dbl>      <dbl> <dbl>     <dbl> <chr> <dbl> <lgl>
#> 1 ALB    2000      1 Europ… 0.242      0.272  74.0     0.752 hpop…  38.6 NA   
#> 2 ALB    2001      2 Europ… 0.255      0.287  74.3     0.759 hpop…  38.4 NA   
#> 3 ALB    2002      3 Europ… 0.242      0.272  74.6     0.766 hpop…  38.3 NA   
#> 4 ALB    2003      4 Europ… 0.243      0.274  74.8     0.771 hpop…  38.1 NA   
#> 5 ALB    2004      5 Europ… 0.254      0.285  75.0     0.776 hpop…  37.9 NA   
#> 6 ALB    2005      6 Europ… 0.261      0.293  75.2     0.780 hpop…  37.8 NA   
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
#>    iso3   year value  pred lower upper source          type     
#>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>           <chr>    
#>  1 ALB    2016  39.8  39.9  39.6  40.1 WHO GHO         estimated
#>  2 ALB    2017  39.9  39.9  39.6  40.1 WHO GHO         estimated
#>  3 ALB    2018  39.9  39.9  39.2  40.6 augury modeling projected
#>  4 ALB    2019  39.9  39.9  38.7  41.1 augury modeling projected
#>  5 ALB    2020  39.9  39.9  38.1  41.7 augury modeling projected
#>  6 ALB    2021  39.9  39.9  37.4  42.4 augury modeling projected
#>  7 ALB    2022  39.9  39.9  36.7  43.2 augury modeling projected
#>  8 ALB    2023  39.9  39.9  35.8  44.0 augury modeling projected
#>  9 ALB    2024  39.9  39.9  35.0  45.0 augury modeling projected
#> 10 ALB    2025  39.9  39.9  34.0  46.0 augury modeling projected
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
#>    iso3   year value  pred lower upper source          type     
#>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>           <chr>    
#>  1 ALB    2016  39.8  39.9  39.6  40.1 WHO GHO         estimated
#>  2 ALB    2017  39.9  39.9  39.6  40.1 WHO GHO         estimated
#>  3 ALB    2018  39.9  39.9  39.2  40.6 augury modeling projected
#>  4 ALB    2019  39.9  39.9  38.7  41.1 augury modeling projected
#>  5 ALB    2020  39.9  39.9  38.1  41.7 augury modeling projected
#>  6 ALB    2021  39.9  39.9  37.4  42.4 augury modeling projected
#>  7 ALB    2022  39.9  39.9  36.7  43.2 augury modeling projected
#>  8 ALB    2023  39.9  39.9  35.8  44.0 augury modeling projected
#>  9 ALB    2024  39.9  39.9  35.0  45.0 augury modeling projected
#> 10 ALB    2025  39.9  39.9  34.0  46.0 augury modeling projected
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
#>   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>                               <chr> 
#> 1 AFG    2018  35    38.6  35.3  41.9 Electronic State Parties Self-Asse… repor…
#> 2 AFG    2019  43    39.3  36.0  42.6 Electronic State Parties Self-Asse… repor…
#> 3 AFG    2020  39.9  39.9  36.6  43.2 WHO DDI Preliminary infilling and … proje…
#> 4 AFG    2021  40.5  40.5  37.2  43.9 WHO DDI Preliminary infilling and … proje…
#> 5 AFG    2022  41.1  41.1  37.8  44.4 WHO DDI Preliminary infilling and … proje…
#> 6 AFG    2023  41.7  41.7  38.4  45.0 WHO DDI Preliminary infilling and … proje…
#> 7 AFG    2024  42.2  42.2  39.0  45.6 WHO DDI Preliminary infilling and … proje…
#> 8 AFG    2025  42.8  42.8  39.6  46.1 WHO DDI Preliminary infilling and … proje…
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
#>   iso3   year ind   value  lower  upper source type  other_detail   pred
#>   <chr> <int> <chr> <dbl>  <dbl>  <dbl> <lgl>  <chr> <lgl>         <dbl>
#> 1 USA    2012 bp    0.999 NA     NA     NA     <NA>  NA           NA    
#> 2 USA    2013 bp    0.999 NA     NA     NA     <NA>  NA           NA    
#> 3 USA    2014 bp    0.999 NA     NA     NA     <NA>  NA           NA    
#> 4 USA    2015 bp    0.999 NA     NA     NA     <NA>  NA           NA    
#> 5 USA    2016 <NA>  0.999  0.999  0.999 NA     <NA>  NA            0.999
#> 6 USA    2017 <NA>  0.999  0.999  0.999 NA     <NA>  NA            0.999
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

predict_holt(bad_df, "x")
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
