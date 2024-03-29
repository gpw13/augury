## code to prepare `covariates_df` dataset goes here
library(dplyr)

load("data-raw/functions_covariates.RDa")

covariates_df <- functions_covariates$covariates.df %>%
  as_tibble() %>%
  transmute(iso3,
            year,
            year_n = year - 1999,
            region,
            un_subregion = whoville::iso3_to_regions(iso3, "un_subregion"),
            gbd_subregion = whoville::iso3_to_regions(iso3, "gbd_subregion"),
            sdg_subregion = whoville::iso3_to_regions(iso3, "sdg_subregion"),
            sdi,
            sdi_scaled = scales::rescale(sdi, c(0,1)),
            e0,
            e0_scaled = scales::rescale(e0, c(0, 1)))

usethis::use_data(covariates_df, overwrite = TRUE)
