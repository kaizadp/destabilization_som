

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

combined_data_key = 
  core_key %>% 
  dplyr::select(-skip) %>% 
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
  combined_data_key %>% 
  filter(type %in% c("control")) %>% 
  group_by(fraction) %>% 
  dplyr::summarise(d13C_VPDB = mean(d13C_VPDB),
                   C_mg_g = mean(C_mg_g)) %>% 
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
  ggplot(aes(x = treatment, y = C13_mg_g, color = fraction))+
  #geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
  geom_point(size = 3, show.legend = FALSE) +
  scale_color_manual(values = pnw_palette("Sailboat", 3))+
  labs(title = "∂13C enrichment in each fraction",
       caption = "dashed line = avg of control samples")+
  facet_grid(fraction~., scales = "free_y")+
  theme_kp()+
  NULL
