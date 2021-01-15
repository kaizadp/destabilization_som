# SOM DESTABILIZATION
# KAIZAD F. PATEL
# Aug 31, 2020

#################### #
#################### #

# 4-processing_plan

## This script contains {drake} plans to process/clean and analyze respiration and soil/weoc IRMS data.
## You `source` scripts # 2 and 3 that contain the necessary functions.
## When you run `make()`, it will only run parts of the script that have changed since the last run. 

## The "processing" plans should generally not change once they've been run.
## The "analysis" plan may change over time, as we add new analyses/graphs. 
## The "analysis" plan is also linked to the RMarkdown report in the `reports` directory. 
## When you `make()` the analysis plan, it will run the RMarkdown document and populate the necessary chunks. 

#################### #
#################### #

source("code/0-packages.R")
source("code/2a-respiration_processing_functions.R")
source("code/2b-irms_processing_functions.R")


# respiration processing plan ---------------------------------------------------------------------
respiration_processing_plan = 
  drake_plan(
    core_key = read.csv(file_in("data/core_key.csv")) %>%  mutate(core = as.character(core)) %>% 
      filter(skip != "skip"),
    #headspace = read.csv("data/respiration_headspace.csv"),
    headspace = read.csv("data/core_weights.csv") %>% dplyr::select(core, headspace_cm3) %>% 
      mutate(core = as.character(core)),
    core_weights = read.csv("data/processed/core_weights.csv") %>% mutate(core = as.character(core)),
    resp_lgr = read.csv("data/respiration_lgr_output.csv"),
    resp_licor = read.csv("data/respiration_licor_output.csv"),
    
    lgr_clean = clean_lgr_output(resp_lgr),
    #lgr_calc = calculate_RC13(lgr_clean),
    
    licor_clean = clean_licor_output(resp_licor),
    licor_moles = calculate_moles_CO2C(headspace, licor_clean, core_weights),
    
    combined_lgr_licor = left_join(lgr_clean, licor_moles, by = "core"), 
      # %>% 
      # mutate(umol_12CO2C = umol_CO2C/(RC13 + 1),
      #        umol_13CO2C = umol_CO2C - umol_12CO2C) %>% 
      # left_join(select(core_key, core, type, treatment), by = "core") %>% 
      # filter(!is.na(treatment)),
    
    respiration_output = combined_lgr_licor %>% 
      write.csv(RESPIRATION_PROCESSED, row.names = F)
  )

make(respiration_processing_plan)


#
# irms processing plan ----------------------------------------------------

soilc_plan = 
  drake_plan(
    # 1. LOAD FILES --------------------------------------------------------------
    C_VPDB = 0.011237,
    core_key = read.csv(file_in(COREKEY)),
    
    # soil
    soil_traykey = read.csv(file_in(IRMS_SOIL_TRAYKEY)) %>% mutate(name = paste0("Tray", tray, "-", position)) %>% dplyr::select(name, core),
    irms_soil_report = read.csv(file_in(IRMS_SOIL_REPORT)),
    tc_soil_report = read.csv(file_in(TC_SOIL_REPORT)),
    
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
    # gg_weoc = plot_weoc(weoc_combined, core_key),
    
    ## c. output
    irms_weoc_processed = process_weoc_files(irms_weoc_report, tc_weoc_report, weoc_traykey)$irms_weoc_wide %>% 
      write.csv("data/processed/weoc_irms_wide.csv", row.names = F),
    tc_weoc_processed = process_weoc_files(irms_weoc_report, tc_weoc_report, weoc_traykey)$tc_weoc_wide %>% 
      write.csv("data/processed/weoc_tc_wide.csv", row.names = F),
    weoc_combined %>% 
      write.csv(WEOC_PROCESSED, row.names = F),
    
    #
    # 3. SOIL -----------------------------------------------------------------
    ## a. process
    soil_combined = process_soil_files(irms_soil_report, tc_soil_report, soil_traykey)$soil_combined,
    soil_calibration = do_calibration(tc_soil_report),
    
    ## b. plot
    # gg_soil = plot_soilc(soil_combined, core_key),
    
    ## c. output
    soil_combined %>% 
      write.csv(SOIL_PROCESSED, row.names = F),
    
    
    # REPORT ------------------------------------------------------------------
 #   report = rmarkdown::render(
 #     knitr_in("reports/irms_report.Rmd"),
 #     output_format = rmarkdown::github_document()), quiet = T
  )



make(soilc_plan)





#################### ####
#################### ####

# ALL DATA COMBINED -------------------------------------------------------

analysis_plan = drake_plan()

soil = read.csv(file_in(SOIL_PROCESSED))
weoc = read.csv(file_in(WEOC_PROCESSED))
respiration = read.csv(file_in(RESPIRATION_PROCESSED))

combined_data = combine_data(soil, weoc, respiration)



core_weights = read.csv("data/processed/core_weights.csv") %>% mutate(core = as.character(core))


 


all_data_combined %>% 
  ggplot(aes(x = treatment, y = C_mg_g, color = type))+
  geom_point(position = position_dodge(width = 0.4))+
  facet_grid(fraction ~ ., scales = "free_y")



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



