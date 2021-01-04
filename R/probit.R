probit_tf <- function(x){
  ####################################################
  # Probit transformation for bounded data
  # Similar to logit but better behaved at the extremes
  x[x <= 1e-4] = 1e-4; x[x >= 1-1e-3] = 1-1e-3
  VGAM::probitlink(x)
}

probit_itf <- function(x){
  ####################################################
  # INVERSE of probit transformation for bounded data
  # Similar to logit but better behaved at the extremes
  VGAM::probitlink(x, inverse = T)
}

time_series <- function(dm, modl){

  dfg    = dm
  isg    = sort(unique(dfg$iso3))
  nsg    = length(isg)
  dfgpr  = list(dim = nsg)

  ncs    = length(unique(dfg$iso3))
  print(paste("Using time-series and", modl, "to forecast", ncs, "countries:"))

  for (ig in 1:nsg){
    #print(paste(isg[ig], ig, "of", nsg))
    dmod        <- dfg %>% filter(iso3 == isg[ig]) %>%
      arrange(year) %>% mutate(x = year - 1999)

    # Set time-series model
    formula     <- y ~ f(x, model = modl)

    result      <-  inla(formula, data=dmod,  family="gaussian",
                         control.predictor= list(compute=TRUE),
                         control.inla = list(strategy = "laplace"))

    fitm        <- result$summary.fitted.values$mean
    fitl        <- result$summary.fitted.values$"0.025quant"
    fitu        <- result$summary.fitted.values$"0.975quant"

    pred.df   <- cov %>% filter(iso3 == isg[ig]) %>%
      select(location_name, iso3, region, year) %>% arrange(year) %>%
      mutate(pred = probit.itf(fitm), lwr = probit.itf(fitl), uppr = probit.itf(fitu))
    dfgpr[[ig]] <- pred.df
  }
  rbindlist(dfgpr)
}

mixed_effects <- function(dm, dfg, modl, my = 1999){
  ncs = length(unique(dfg$iso3))
  print(paste("Using covariates with", modl, "to forecast", ncs, "countries:"))
  dmod         <- dm %>%
    arrange(iso3, year) %>%
    filter(year > my) %>%
    mutate(t = year - my,
           msdi = min(sdi, na.rm = T), sdsdi = max(sdi, na.rm = T) - msdi,
           sdib = (sdi - msdi)/sdsdi,
           me0  = min(e0, na.rm = T), sde0 = max(e0, na.rm = T) - me0,
           e0b  = (e0 - me0)/sde0)

  formula     <- y ~ sdib + e0b + f(region, model = 'iid') + f(t, model=modl)

  result      <-  inla(formula, data=dmod,  family="gaussian",
                       control.predictor= list(compute=TRUE),
                       control.inla = list(strategy = "laplace"))

  fitm        <- result$summary.fitted.values$mean
  fitl        <- result$summary.fitted.values$"0.025quant"
  fitu        <- result$summary.fitted.values$"0.975quant"

  pred.df     <- tibble(dmod %>%  select(location_name, iso3, region, year, y),
                        pred = fitm, lwr = fitl, uppr = fitu) %>%
    mutate(es   = y - pred) %>%
    group_by(iso3) %>% mutate(es = mean(es, na.rm = T)) %>% ungroup() %>%
    mutate(es1  = mean(es, na.rm = T)) %>% ungroup() %>%
    mutate(es   = ifelse(is.na(es), es1, es)) %>%
    filter(iso3 %in% unique(dfg$iso3)) %>%
    arrange(iso3, year) %>%
    mutate(pred = probit.itf(pred + es),
           lwr  = probit.itf(lwr + es),
           uppr = probit.itf(uppr + es)) %>%
    select(location_name, iso3, region, year, pred, lwr, uppr)

  pred.df
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
