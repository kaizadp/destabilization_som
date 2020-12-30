# RESPIRATION

# respiration processing plan ---------------------------------------------------------------------
respiration_processing_plan = 
  drake_plan(
    core_key = read.csv(file_in("data/core_key.csv")) %>% mutate(core = as.character(core)) %>% filter(skip != "skip"),
    #headspace = read.csv("data/respiration_headspace.csv"),
    headspace = read.csv("data/core_weights.csv") %>% dplyr::select(core, headspace_cm3) %>% 
      mutate(core = as.character(core)),
    resp_lgr = read.csv("data/respiration_lgr_output.csv"),
    resp_licor = read.csv("data/respiration_licor_output.csv"),
    
    lgr_clean = clean_lgr_output(resp_lgr),
    lgr_calc = calculate_RC13(lgr_clean),
    
    licor_clean = clean_licor_output(resp_licor),
    licor_moles = calculate_moles_CO2C(headspace, licor_clean),
    
    combined_lgr_licor = 
      lgr_calc %>% 
      left_join(licor_moles, by = "core") %>% 
      mutate(umol_12CO2C = umol_CO2C/(RC13 + 1),
             umol_13CO2C = umol_CO2C - umol_12CO2C) %>% 
      left_join(select(core_key, core, type, treatment), by = "core") %>% 
      filter(!is.na(treatment)),
    
    respiration_output = combined_lgr_licor %>% 
      write.csv("data/processed/respiration.csv", row.names = F)
  )

make(respiration_processing_plan)



# respiration analysis plan -----------------------------------------------

respiration_analysis_plan = 
  drake_plan(
    theme_set(theme_bw()),
    respiration = read.csv(file_in("data/processed/respiration.csv")),
    priming_table = make_priming_table(respiration) %>% knitr::kable(),
    gg_respiration = plot_respiration(respiration),
    
    # report
    report = rmarkdown::render(
      knitr_in("reports/respiration_drake_report.Rmd"),
      output_format = rmarkdown::github_document())
  )

make(respiration_analysis_plan)




source("code/0-packages.R")
source("code/3-irms_processing.R")

soilc_plan = 
  drake_plan(
    # 1. LOAD FILES --------------------------------------------------------------
    C_VPDB = 0.011237,
    core_key = read.csv(file_in(COREKEY)),
    
    # soil
    soil_traykey = read.csv(file_in(IRMS_SOIL_TRAYKEY)) %>% mutate(name = paste0("Tray", tray, "-", position)) %>% dplyr::select(name, core),
    irms_soil_report = read.csv(file_in(IRMS_SOIL_REPORT)),
    tc_soil_report = read.csv(TC_SOIL_REPORT),
    
    # weoc
    weoc_traykey = read.csv(file_in(IRMS_WEOC_TRAYKEY)) %>% mutate(name = paste0("Tray", tray, "-", position)) %>% dplyr::select(name, core, weoc_rep),
    irms_weoc_report = read.csv(file_in(IRMS_WEOC_REPORT)),
    tc_weoc_report = read.csv(file_in(TC_WEOC_REPORT)),
    weoc_subsampling = read.csv(file_in(WEOC_SUBSAMPLING)),
    weoc_capsuleweights = read.csv(file_in(WEOC_CAPSULES)),
    
    #
    # 2. WEOC -----------------------------------------------------------------
    ## a. process
    weoc_combined = process_weoc_files(irms_weoc_report, tc_weoc_report, weoc_traykey)$weoc_combined,
    weoc_calibration = do_calibration(tc_weoc_report),
    
    ## b. plot
    gg_weoc = plot_weoc(weoc_combined, core_key),
    
    ## c. output
    irms_weoc_processed = process_weoc_files(irms_weoc_report, tc_weoc_report, weoc_traykey)$irms_weoc_wide %>% 
      write.csv("data/processed/weoc_irms_wide.csv", row.names = F),
    tc_weoc_processed = process_weoc_files(irms_weoc_report, tc_weoc_report, weoc_traykey)$tc_weoc_wide %>% 
      write.csv("data/processed/weoc_tc_wide.csv", row.names = F),
    weoc_combined %>% 
      write.csv("data/processed/weoc_combined.csv", row.names = F),
    
    #
    # 3. SOIL -----------------------------------------------------------------
    ## a. process
    soil_combined = process_soil_files(irms_soil_report, tc_soil_report, soil_traykey)$soil_combined,
    soil_calibration = do_calibration(tc_soil_report),
    
    ## b. plot
    gg_soil = plot_soilc(soil_combined, core_key),
    
    ## c. output
    soil_combined %>% 
      write.csv("data/processed/soil_combined.csv", row.names = F),
    
    
    # REPORT ------------------------------------------------------------------
    report = rmarkdown::render(
      knitr_in("reports/irms_report.Rmd"),
      output_format = rmarkdown::github_document()), quiet = T
  )



make(soilc_plan)
