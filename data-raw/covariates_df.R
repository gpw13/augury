## code to prepare `covariates_df` dataset goes here
library(dplyr)

covariates_df <- readr::read_csv("data-raw/covariates_df.csv") %>%
  select(iso3,
            year,
            region,
            sdi,
            cdr,
            e0,
            q5,
            q1)

usethis::use_data(covariates_df, overwrite = TRUE)
