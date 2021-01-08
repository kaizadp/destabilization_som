

# combine files -----------------------------------------------------------

combine_data = function(soil, weoc, respiration){
  soil_new = 
    soil %>% 
    rename(C_mg_g = totalC_mg_g) %>% 
    mutate(fraction = "soil") %>% 
    dplyr::select(core, fraction, C_mg_g, d13C_VPDB) %>% 
    mutate(core = as.character(core))
  
  weoc_new = 
    weoc %>% 
    rename(C_mg_g = weoc_mg_g) %>% 
    mutate(fraction = "weoc") %>% 
    dplyr::select(core, fraction, C_mg_g, d13C_VPDB) %>% 
    mutate(core = as.character(core))
  
  respiration_new = 
    respiration %>% 
    rename(C_mg_g = CO2C_mg_g,
           d13C_VPDB = D13C_VPDB_CO2) %>% 
    mutate(fraction = "respiration") %>% 
    dplyr::select(core, fraction, C_mg_g, d13C_VPDB)
  
  bind_rows(soil_new, weoc_new, respiration_new) %>% 
    mutate(d13C_VPDB = round(d13C_VPDB,3)) %>% 
    filter(core != 40) %>% 
    drop_na()
  
}
core_key = read.csv(file_in("data/core_key.csv")) %>% mutate(core = as.character(core)) %>% filter(skip != "skip")

combined_data_key = 
  core_key %>% 
  dplyr::select(-skip) %>% 
  left_join(core_weights_processed %>% dplyr::select(core, od_soil_g) %>% mutate(core = as.character(core)), by = "core") %>% 
  left_join(combined_data, by = "core") 

R13C_VPDB = 0.011237

combined_data_processed = 
  combined_data_key %>% 
  mutate(R13C = ((d13C_VPDB/1000) + 1) * R13C_VPDB,
         F13C = R13C/(1+R13C),
         R13C = round(R13C, 4),
         F13C = round(F13C, 4),
         C13_mg_g = F13C * C_mg_g)
  # convert delta into R and F




# graphs ------------------------------------------------------------------



control_summary = 
  combined_data_processed %>% 
  filter(type %in% c("control")) %>% 
  group_by(fraction) %>% 
  dplyr::summarise(d13C_VPDB = mean(d13C_VPDB),
                   C_mg_g = mean(C_mg_g),
                   C13_mg_g = mean(C13_mg_g)) %>% 
  mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
  

combined_data_key %>% 
  mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
  filter(type %in% c("desorption")) %>% 
  ggplot(aes(x = treatment, y = d13C_VPDB, color = fraction))+
  geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
  geom_point(size = 3, show.legend = FALSE) +
  scale_color_manual(values = pnw_palette("Sailboat", 3))+
  labs(title = "∂13C enrichment in each fraction",
       caption = "dashed line = avg of control samples")+
  facet_grid(fraction~., scales = "free_y")+
  theme_kp()+
  NULL

combined_data_processed %>% 
  mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
  filter(type %in% c("desorption")) %>% 
  ggplot(aes(x = treatment, y = C_mg_g, color = fraction))+
  geom_hline(data = control_summary, aes(yintercept = C_mg_g), linetype = "dashed", color = "grey30")+
  geom_point(size = 3, show.legend = FALSE) +
  scale_color_manual(values = pnw_palette("Sailboat", 3))+
  labs(title = "total carbon in each fraction (mg/g)",
       caption = "dashed line = avg of control samples")+
  facet_grid(fraction~., scales = "free_y")+
  theme_kp()+
  NULL

combined_data_processed %>% 
  mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
  filter(type %in% c("desorption")) %>% 
  ggplot(aes(x = treatment, y = C13_mg_g, color = fraction, shape = type))+
  geom_hline(data = control_summary, aes(yintercept = C13_mg_g), linetype = "dashed", color = "grey30")+
  geom_point(size = 3, show.legend = FALSE) +
  scale_color_manual(values = pnw_palette("Sailboat", 3))+
  labs(title = "∂13C enrichment in each fraction",
       caption = "dashed line = avg of control samples")+
  facet_grid(fraction~., scales = "free_y")+
  theme_kp()+
  NULL


combined_data_processed %>% 
  mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
  filter(type %in% c("desorption")) %>% 
  ggplot(aes(x = treatment, y = F13C, color = fraction))+
  #geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
  geom_point(size = 3, show.legend = FALSE) +
  scale_color_manual(values = pnw_palette("Sailboat", 3))+
  labs(title = "∂13C enrichment in each fraction",
       caption = "dashed line = avg of control samples")+
  facet_grid(fraction~., scales = "free_y")+
  theme_kp()+
  NULL


combined_data_processed %>% 
  mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
  filter(type %in% c("priming")) %>% 
  ggplot(aes(x = treatment, y = C13_mg_g*od_soil_g, color = fraction))+
  #geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
  geom_point(size = 3, show.legend = FALSE) +
  scale_color_manual(values = pnw_palette("Sailboat", 3))+
  labs(title = "C13 content of each experimental unit",
       caption = "dashed line = avg of control samples",
       y = "C13 (mg)")+
  facet_grid(fraction~., scales = "free_y")+
  theme_kp()+
  NULL



# -------------------------------------------------------------------------

# CONTROL -----------------------------------------------------------------


## what is the isotope composition of control soil?
core_key = read.csv(file_in("data/core_key.csv")) %>% mutate(core = as.character(core)) %>% filter(skip != "skip")

control = 
  core_weights_processed %>% 
  dplyr::select(core, od_soil_g) %>% 
  left_join(tc_soil_combined %>% rename(tc_d13C_VPDB = d13C_VPDB, tc_R13C = R13C), 
            by = "core") %>% 
  left_join(tc_weoc_combined %>% rename(weoc_d13C_VPDB = d13C_VPDB, weoc_R13C = R13C), 
            by = "core") %>% 
  mutate(core = as.character(core)) %>% 
  left_join(core_key, by = "core") %>% 
  filter(type == "control")


control = 
  combined_data_key %>% 
  filter(type == "control")

CONTROL_TC_D13C = 
  control %>% 
  filter(treatment == "1-time-zero" & fraction == "soil") %>% 
  dplyr::summarise(d13C_VPDB = mean(d13C_VPDB)) %>% 
  pull(d13C_VPDB)

CONTROL_WEOC_D13C = 
  control %>% 
  filter(treatment == "1-time-zero" & fraction == "weoc") %>% 
  dplyr::summarise(d13C_VPDB = mean(d13C_VPDB)) %>% 
  pull(d13C_VPDB)


        a = 
          licor_clean %>% 
          left_join(dplyr::select(headspace_calc, core, mmol_air), by = "core") %>% 
          mutate(mmol_CO2C = CO2_ppm * mmol_air/1000000,
                 umol_CO2C = round(mmol_CO2C * 1000,3)) %>% 
          dplyr::select(core, CO2_ppm, umol_CO2C) %>% 
          left_join(core_key, by = "core") 
        
        a %>% 
          ggplot(aes(x = treatment, y = CO2_ppm, color = type))+
          geom_point(position = position_dodge(width = 0.4))
        
        summary(
          aov(CO2_ppm ~ type, 
              data = 
                a %>% 
                filter(type %in% c("control", "priming") &
                         treatment == "1-time-zero" &
                         core != 40))
        )
        
        a_priming = 
          a %>% 
          filter(type %in% "priming" & treatment == "1-time-zero")
        
        dixon.test(a_priming %>% pull(CO2_ppm),
                   type = 0, opposite = FALSE, two.sided = TRUE) %>% 
          summary()
        
        
        
        dixon.outliers(set20)
        
        fit_dixon = function(dat){
          
          dixon.test(dat %>% pull(CO2_ppm),
                     type = 10, opposite = FALSE, two.sided = TRUE) %>% broom::tidy() 
        }
        
        
        a_dixon = 
          a %>% filter(core != 40) %>% 
          group_by(type, treatment) %>% 
          do(fit_dixon(.))
        

# mixing model for respiration --------------------------------------------


OXALIC_D13C = 15098.6 # permille

resp_fraction = 
  combined_data_key %>% 
  filter(fraction == "respiration") %>% 
  mutate(f_oxalic = (d13C_VPDB - CONTROL_WEOC_D13C)/(OXALIC_D13C - CONTROL_WEOC_D13C))

resp_fraction %>% 
  filter(type == "priming") %>% 
  ggplot(aes(x = treatment, y = f_oxalic * 100, color = type))+
  geom_point()+
  labs(title = "% contribution of oxalic-13C to respiration")

summary(aov(D13C_calc ~ treatment,
            data = combined_lgr_licor %>% 
              filter(type == "desorption" & treatment %in% c("2-wetting", "4-drying-rewetting"))
))




# -------------------------------------------------------------------------


