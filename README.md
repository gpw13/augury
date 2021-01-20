
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

Given that models might need to be fit separately to different portions
of a data frame, such as fitting time series models for each country
individually, there are grouped versions of the above functions.

-   `grouped_predict_general_mdl()`
-   `grouped_predict_inla()`

The grouped functions apply the modeling separately across each group
before joining them back together into the original data frame and
returning similar objects as the original functions.

## Additional model wrappers

Additional functions are provided as requested to streamlining fitting
of specific models. For general modeling, there are grouped and
individual functions for `stats::lm()`, `stats::glm()`, and
`lme4::lmer()` to fit linear models, generalized linear models, and
linear mixed-effects models respectively. These wrappers are:

-   `predict_lm()` & `grouped_predict_lm()`
-   `predict_glm()` & `grouped_predict_glm()`
-   `predict_lmer()` & `grouped_predict_lmer()`

As well, wrappers are provided around INLA for models currently in place
for the Triple Billion framework. These are a time series model with no
additional covariates (fit individually by country) and a mixed-effects
model using covariates.

-   `predict_inla_me()` & `grouped_predict_inla_me()`

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

``` r
modeled_df <- df %>%
  scale_transform("value") %>%
  probit_transform("value") %>%
  grouped_predict_inla_ts(type_col = "type",
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
#>  1 ALB    2016  39.8  39.9  40.1  39.6 WHO GHO         estimated
#>  2 ALB    2017  39.9  39.9  40.1  39.6 WHO GHO         estimated
#>  3 ALB    2018  39.9  39.9  40.6  39.2 augury modeling projected
#>  4 ALB    2019  39.9  39.9  41.1  38.7 augury modeling projected
#>  5 ALB    2020  39.9  39.9  41.7  38.1 augury modeling projected
#>  6 ALB    2021  39.9  39.9  42.4  37.4 augury modeling projected
#>  7 ALB    2022  39.9  39.9  43.2  36.7 augury modeling projected
#>  8 ALB    2023  39.9  39.9  44.0  35.8 augury modeling projected
#>  9 ALB    2024  39.9  39.9  45.0  35.0 augury modeling projected
#> 10 ALB    2025  39.9  39.9  46.0  34.0 augury modeling projected
```

And there we go, we have now fit a time series model to our data,
provided new type and source, and merged this into our existing data
frame.

## Mixed-effects modeling

Now we will look at another indicator, a composite of 13 International
Health Regulations core capacity scores, SPAR version. Since countries
only have two data points at most, we will use mixed-effects modeling to
infill and project the data.

``` r
df <- ghost::gho_data("SDGIHR2018") %>%
  billionaiRe::wrangle_gho_data(source = "WHO GHO",
                                type = "reported")

head(df)
#> # A tibble: 6 x 9
#>   iso3   year ind   value lower upper source  type     other_detail
#>   <chr> <int> <chr> <dbl> <lgl> <lgl> <chr>   <chr>    <lgl>       
#> 1 AFG    2018 espar    35 NA    NA    WHO GHO reported NA          
#> 2 AFG    2019 espar    43 NA    NA    WHO GHO reported NA          
#> 3 AGO    2018 espar    59 NA    NA    WHO GHO reported NA          
#> 4 AGO    2019 espar    63 NA    NA    WHO GHO reported NA          
#> 5 ALB    2018 espar    NA NA    NA    WHO GHO reported NA          
#> 6 ALB    2019 espar    62 NA    NA    WHO GHO reported NA
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
                  source = "augury modeling") %>%
  probit_transform(c("value", "pred", "upper", "lower"), inverse = TRUE) %>%
  scale_transform(c("value", "pred", "upper", "lower"), divide = FALSE)

# Look at an example for Afghanistan

modeled_df %>%
  filter(year > 2017, iso3 == "AFG") %>%
  select(iso3, year, value, pred, lower, upper, source, type)
#> # A tibble: 8 x 8
#>   iso3   year value  pred lower upper source          type     
#>   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>           <chr>    
#> 1 AFG    2018  35    42.3  45.7  38.9 WHO GHO         reported 
#> 2 AFG    2019  43    43.0  46.4  39.7 WHO GHO         reported 
#> 3 AFG    2020  43.6  43.6  47.0  40.2 augury modeling projected
#> 4 AFG    2021  44.2  44.2  47.6  40.9 augury modeling projected
#> 5 AFG    2022  44.8  44.8  48.2  41.5 augury modeling projected
#> 6 AFG    2023  45.5  45.5  48.8  42.1 augury modeling projected
#> 7 AFG    2024  46.0  46.0  49.3  42.7 augury modeling projected
#> 8 AFG    2025  46.6  46.6  49.9  43.3 augury modeling projected
```

And exactly as we were able to do with the time series modeling, we now
have infilled missing data for this indicator using mixed-effects
modeling in INLA.

## Building further

Building further on this work, you can tweak any of the arguments passed
to these INLA models or use the base `predict_inla()` and
`grouped_predict_inla()` and other covariates to test and compare other
models. There is much more functionality to test modeling accuracy and
iteratively develop methods available in this package not shown here, so
please continue to explore and play around.
