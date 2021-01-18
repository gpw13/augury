## code to prepare `covariates_df` dataset goes here
library(dplyr)

covariates_df <- readr::read_csv("data-raw/covariates_df.csv") %>%
  transmute(iso3,
            year,
            year_n = year - 1999,
            region,
            sdi,
            sdi_scaled = scales::rescale(sdi, c(0,1)),
            e0,
            e0_scaled = scales::rescale(e0, c(0, 1)))

usethis::use_data(covariates_df, overwrite = TRUE)

