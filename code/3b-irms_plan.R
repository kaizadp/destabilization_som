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
    
    # REPORT ------------------------------------------------------------------
    report = rmarkdown::render(
      knitr_in("reports/irms_report.Rmd"),
      output_format = rmarkdown::github_document()), quiet = T
  )



make(soilc_plan)
