# SOM DESTABILIZATION
# KAIZAD F. PATEL
# Nov 29, 2020

#################### #
#################### #

# 3-irms_processing

## This script contains functions needed to process IRMS/ totalC reports.

#################### #
#################### #
C_VPDB = 0.011237

# PROCESSING FUNCTIONS -------------------------------------------------------------------------
do_calibration = function(dat){
  
  calib = 
    dat %>% 
    filter(Name %in% c("MT", "aspartic")) %>% 
    dplyr::select(Name, weight_mg, C_area, C_perc) %>% 
    mutate(C_mg = weight_mg * C_perc/100)
  
  calib_coeff = 
    calib %>% 
    dplyr::summarize(slope = lm(C_mg~C_area)$coefficients["C_area"], 
                     intercept = lm(C_mg~C_area)$coefficients["(Intercept)"])
  
  calib_plot = 
    calib %>% 
    ggplot(aes(x = C_area, y = C_mg))+
    geom_point()+
    geom_smooth(method = "lm", se = F)
  
  
  list(slope = calib_coeff %>% pull(slope),
       intercept = calib_coeff %>% pull(intercept),
       calib_plot = calib_plot)
}

process_weoc_files = function(irms_weoc_report, tc_weoc_report){
  
  # 1. IRMS --------------------------------------------------------------------
  ## a. clean and get d13C, R13/12C values for all reps
  irms_weoc_allreps = 
    irms_weoc_report %>% 
    dplyr::select(name, sample_group, d13C_VPDB) %>% 
    filter(sample_group == "samples") %>% 
    left_join(weoc_traykey, by = "name") %>% 
    filter(!is.na(core)) %>% 
    mutate(
      R13C = ((d13C_VPDB/1000) + 1) * C_VPDB,
      R13C = round(R13C, 6)) %>% 
    dplyr::select(core, weoc_rep, d13C_VPDB, R13C)
  
  ## b. make wide form with measures of variance
  irms_weoc_cv = 
    irms_weoc_allreps %>% 
    dplyr::select(core, weoc_rep, d13C_VPDB) %>% 
    group_by(core) %>% 
    dplyr::summarise(sd = round(sd(d13C_VPDB), 2),
                     cv = round(sd/mean(d13C_VPDB), 2),
                     cv = abs(cv))
  
  irms_weoc_wide = 
    irms_weoc_allreps %>% 
    dplyr::select(core, weoc_rep, d13C_VPDB) %>% 
    pivot_wider(names_from = "weoc_rep", values_from = "d13C_VPDB") %>% 
    left_join(irms_weoc_cv, by = "core") %>% 
    rename(d13C_A = A,
           d13C_B = B,
           d13C_C = C) %>% 
    arrange(core)
  
  #  irms_weoc_outliers = 
  #    irms_weoc %>% 
  #    dplyr::select(core, weoc_rep, d13C_VPDB) %>% 
  #    group_by(core) %>% 
  #    dplyr::mutate(sd = round(sd(d13C_VPDB), 2),
  #                  cv = round(sd/mean(d13C_VPDB), 2),
  #                  cv = abs(cv),
  #                  mean = mean(d13C_VPDB),
  #                  out = d13C_VPDB-mean,
  #                  outlier = (d13C_VPDB-mean) > (3*sd))
  #  
  
  ## c. get WEOC summary (don't do???)
  irms_weoc_summary = 
    irms_weoc_allreps %>% 
    ungroup() %>% 
    group_by(core) %>% 
    dplyr::summarize(d13C_VPDB = round(mean(d13C_VPDB), 2),
                     R13C = round(mean(R13C), 6))
  
  #  
  
  
  # 2. TC ----------------------------------------------------------------------
  SLOPE_weoc = do_calibration(tc_weoc_report)$slope
  INTERCEPT_weoc = do_calibration(tc_weoc_report)$intercept
  
  ## a. clean data for all reps
  tc_weoc = 
    tc_weoc_report %>% 
    filter(is.na(Memo)) %>% 
    dplyr::select(Name, weight_mg, C_area) %>% 
    mutate(C_mg_calc = round(C_area*SLOPE_weoc + INTERCEPT_weoc, 3)) %>% 
    left_join(weoc_traykey, by = c("Name" = "name")) %>% 
    filter(!is.na(core)) %>% 
    #mutate(core = as.character(core)) %>% 
    dplyr::select(core, weoc_rep, weight_mg, C_mg_calc)
  
  ## now bring in weights data for proper unit conversion
  ## 3.5 g --> 35 mL extract
  ## 2 mL extract --> weight mg
  
  weoc_subsampling2 = 
    weoc_subsampling %>% 
    dplyr::select(core, weoc_g, moisture_perc) %>% 
    drop_na() %>% 
    mutate(core = as.integer(as.character(core)),
           moisture_perc = round(moisture_perc,2),
           weoc_drywt_g = weoc_g/((moisture_perc/100)+1),
           weoc_drywt_g = round(weoc_drywt_g, 2),
           water_g = weoc_g - weoc_drywt_g) %>% 
    dplyr::select(core, weoc_drywt_g, water_g)
  
  weoc_capsuleweights2 = 
    weoc_capsuleweights %>% 
    dplyr::select(core, weoc_rep, wt_filtered_extract_g) %>% 
    filter(weoc_rep %in% c("A", "B", "C")) %>% 
    mutate(core = as.integer(as.character(core))) %>% 
    drop_na()
  
  tc_weoc_allreps = 
    tc_weoc %>% 
    left_join(weoc_capsuleweights2, by = c("core", "weoc_rep")) %>% 
    left_join(weoc_subsampling2, by = "core") %>% 
    mutate(weoc_mg_g = C_mg_calc * (1/wt_filtered_extract_g) * ((35+water_g)/weoc_drywt_g)) %>% 
    #### CHECK FOR VARIANCE !!!
    ####
    dplyr::select(core, weoc_rep, C_mg_calc, weoc_mg_g)
  
  ## b. make wide form with cv, sd
  tc_weoc_cv = 
    tc_weoc_allreps %>% 
    dplyr::select(core, weoc_rep, weoc_mg_g) %>% 
    group_by(core) %>% 
    dplyr::summarise(sd = round(sd(weoc_mg_g), 4),
                     cv = round(sd/mean(weoc_mg_g),4))
  
  tc_weoc_wide = 
    tc_weoc_allreps %>% 
    dplyr::select(core, weoc_rep, weoc_mg_g) %>% 
    mutate(weoc_mg_g = round(weoc_mg_g, 4)) %>% 
    pivot_wider(names_from = "weoc_rep", values_from = "weoc_mg_g") %>% 
    left_join(tc_weoc_cv, by = "core") %>% 
    rename(weoc_mgg_A = A,
           weoc_mgg_B = B,
           weoc_mgg_C = C) %>% 
    arrange(core)
  
  #
  # 3. combined ----------------------------------------------------------------
  weoc_combined_allreps = 
    left_join(tc_weoc_allreps, irms_weoc_allreps, by = c("core", "weoc_rep")) %>% 
    mutate(C13_mg = R13C/(1+R13C))
  
  weoc_combined = 
    weoc_combined_allreps %>% 
    group_by(core) %>% 
    dplyr::summarise(weoc_mg_g = round(mean(weoc_mg_g), 3),
                     d13C_VPDB = round(mean(d13C_VPDB), 2),
                     R13C = round(mean(R13C), 4)
    )
  
  #  
  # 4. outputs ----
  list(irms_weoc_wide = irms_weoc_wide,
       tc_weoc_wide = tc_weoc_wide,
       weoc_combined = weoc_combined
  )
}