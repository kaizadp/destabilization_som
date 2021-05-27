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
ghg_fluxes2 = ghg_fluxes %>% distinct()

ghg_fluxes2 %>% 
  filter(treatment_mgC <= 5) %>% 
  ggplot(aes(x = DATETIME, y = flux_co2_umol_g_s, color = as.character(treatment_mgC)))+
  geom_path(aes(group = treatment_mgC))+
  #scale_color_gradientn(colors = PNWColors::pnw_palette("Sunset2", 7))+
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 3))+
  facet_grid(.~goethite)


ghg_fluxes2_2_5 = 
  ghg_fluxes2 %>% 
  filter(treatment_mgC < 5)

nlme::lme(flux_co2_umol_g_s ~ treatment_mgC, random = ~1|, data = ghg_fluxes2_2_5)



total = ghg_fluxes2 %>% 
  group_by(goethite, treatment_mgC) %>% 
  dplyr::summarise(total = sum(flux_co2_umol_g_s))

a = 
  total %>% 
  filter(goethite == "soil + goethite") %$%
  aov(total ~ treatment_mgC)
summary(a)

agricolae::HSD.test(a, "treatment_mgC") %>% print()


cumflux = 
  ghg_fluxes2 %>% 
  dplyr::select(Core, Sample_number, goethite, treatment_mgC,  DATETIME, flux_co2_umol_g_s) %>% 
  group_by(Core) %>% 
  mutate(next_flux = lead(flux_co2_umol_g_s),
         next_time = lead(DATETIME),
         delta_time = as.numeric(next_time-DATETIME)) %>% 
  drop_na() %>% 
  rowwise() %>% 
  mutate(
         #delta_flux = next_flux - flux_co2_umol_g_s,
         mean_flux = mean(flux_co2_umol_g_s, next_flux, na.rm = TRUE),
         evolved = mean_flux/(delta_time*60)) %>% 
  ungroup() %>% 
  group_by(Core) %>% 
  mutate(cum_evolved_umol_g = cumsum(evolved),
         cum_evolved_ug_g = cum_evolved_umol_g * 12,
         cum_evolved_mg_g = cum_evolved_ug_g/1000)

  
cumflux %>% 
  #filter(treatment_mgC <= 5) %>% 
  ggplot(aes(x = DATETIME, y = cum_evolved_mg_g, color = as.character(treatment_mgC)))+
  geom_path()+
  facet_grid(. ~ goethite)+
  theme_kp()+
  NULL
  
  
  
b = 
  cumflux %>%
  group_by(Core) %>% 
  filter(cum_evolved_mg_g == max(cum_evolved_mg_g)) %>% 
  filter(goethite == "soil + goethite") %$%
  aov(cum_evolved_mg_g ~ treatment_mgC)
summary(b)

agricolae::HSD.test(b, "treatment_mgC") %>% print()  
  
cumflux %>%
  group_by(Core) %>% 
  filter(cum_evolved_mg_g == max(cum_evolved_mg_g)) %$%
  nlme::lme(cum_evolved_mg_g ~ goethite, random = ~1|treatment_mgC) %>% 
  anova()


  
cumflux2 = 
  cumflux %>% 
  group_by(Core) %>% 
  mutate(elapsed_sec = difftime(DATETIME, min(DATETIME)),
         elapsed_hr = as.numeric(elapsed_sec)/3600) %>% 
  mutate(treatment_mgC = as.factor(treatment_mgC),
         treatment_mgC = fct_reorder(treatment_mgC, as.numeric(treatment_mgC)))
  
  
cumflux2 %>% 
  #filter(treatment_mgC <= 5) %>% 
  ggplot(aes(x = elapsed_hr, y = cum_evolved_umol_g*1000, color = (treatment_mgC)))+
  geom_path()+
   scale_color_manual(values = PNWColors::pnw_palette("Sailboat", 7))+
  # scale_color_viridis_d(option = "cividis", direction = -1)+
  labs(title = "Respiration response to glucose addition \n",
       x = "Elapsed hours",
       y = expression(bold("Cumulative CO"[2] * "-C evolved, nmol g" ^-1)),
       color = "mg C added")+
  
  facet_grid(. ~ goethite)+
  theme_kp()+
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0),
        legend.title = element_text()) +
  NULL
  

