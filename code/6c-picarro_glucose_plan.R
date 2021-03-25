library(lubridate)
source("code/6a-picarro_data.R")  
    
    ##    tray=0.08 
    ##    traysoil=6.91 
    ##    traydry=6.67  
    ##      
    ##    ((traysoil-tray) - (traydry-tray)) / (traydry-tray) 
    ##    gravmoisture = 3.64 # %


key = read.csv("data/picarro_corekey.csv", na.strings = "")
valve = read.csv("data/picarro_valvekey.csv", na.strings = "")




picarro_plan = 
  drake_plan(
    
    COREKEY = "data/picarro_corekey.csv",
    VALVEKEY = "data/picarro_valvekey.csv",         
    
    PICARROPATH = "data/picarro_glucose/",
    
    
  core_key = read_core_key(COREKEY),
  valve_key = read_valve_key(VALVEKEY, core_key),
  
  
  picarro_raw = sapply(list.files(path = PICARROPATH, pattern = "dat$", recursive = TRUE,full.names = TRUE),
                       read.table,header=TRUE, simplify = FALSE) %>% bind_rows(),
  
  picarro_clean = clean_picarro_data(picarro_raw),
  
  # Match Picarro data with the valve key data
  pcm = match_picarro_data(picarro_clean, valve_key),
  picarro_clean_matched = pcm$pd,
  picarro_match_count = pcm$pmc,
  valve_key_match_count = pcm$vkmc,
  
  qc1 = qc_match(picarro_clean, picarro_clean_matched, valve_key, picarro_match_count, valve_key_match_count),
  qc2 = qc_concentrations(picarro_clean_matched, valve_key),
  
  pcm_positive = filter_positive_slope(picarro_clean_matched),
  
  ghg_fluxes = compute_ghg_fluxes(pcm_positive, valve_key),
  qc3 = qc_fluxes(ghg_fluxes, valve_key),
  
#  gf = 
#    ghg_fluxes %>% 
#    left_join(core_key) %>% 
#    filter(flux_co2_umol_g_s >= 0) %>% 
#    # remove outliers
#    group_by(Core_assignment) %>% 
#    dplyr::mutate(mean = mean(flux_co2_umol_g_s),
#                  median = median(flux_co2_umol_g_s),
#                  sd = sd(flux_co2_umol_g_s)) %>% 
#    ungroup %>% 
#    dplyr::mutate(outlier = flux_co2_umol_g_s - mean > 4 * sd)
)


make(picarro_plan)


loadd(ghg_fluxes)

ghg_fluxes %>% 
  ggplot(aes(x = DATETIME, y = flux_co2_umol_g_s, color = treatment_mgC))+
  geom_path(aes(group = treatment_mgC))+
  scale_color_gradientn(colors = PNWColors::pnw_palette("Sunset2", 7))+
  facet_grid(.~goethite)

