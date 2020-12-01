source("code/0-packages.R")
source("code/3-irms_processing.R")

C_VPDB = 0.011237

soilc_plan = 
  drake_plan(
    # LOAD FILES --------------------------------------------------------------
    C_VPDB = 0.011237,
    core_key = read.csv(file_in(COREKEY)),
    
    # soil
    soil_traykey = read.csv(file_in(IRMS_SOIL_TRAYKEY)) %>% mutate(name = paste0("Tray", tray, "-", position)) %>% dplyr::select(name, sample_no),
    irms_soil_report = read.csv(file_in(IRMS_SOIL_REPORT)),
#    tc_soil_report = read.csv(TC_SOIL_REPORT),
    
    # weoc
    weoc_traykey = read.csv(file_in(IRMS_WEOC_TRAYKEY)) %>% mutate(name = paste0("Tray", tray, "-", position)) %>% dplyr::select(name, core, weoc_rep),
    irms_weoc_report = read.csv(file_in(IRMS_WEOC_REPORT)),
    tc_weoc_report = read.csv(file_in(TC_WEOC_REPORT)),
    weoc_subsampling = read.csv(file_in(WEOC_SUBSAMPLING)),
    weoc_capsuleweights = read.csv(file_in(WEOC_CAPSULES)),
    
    #
    # PROCESS -----------------------------------------------------------------
    weoc_combined = process_weoc_files(irms_weoc_report, tc_weoc_report)$weoc_combined,
    
    #
    # OUTPUT FILES ------------------------------------------------------------
    irms_weoc_processed = process_weoc_files(irms_weoc_report, tc_weoc_report)$irms_weoc_wide %>% 
      write.csv("data/processed/weoc_irms_wide.csv", row.names = F),
    tc_weoc_processed = process_weoc_files(irms_weoc_report, tc_weoc_report)$tc_weoc_wide %>% 
      write.csv("data/processed/weoc_tc_wide.csv", row.names = F),
    weoc_combined %>% 
      write.csv("data/processed/weoc_combined.csv", row.names = F),
    
  )

make(soilc_plan)
