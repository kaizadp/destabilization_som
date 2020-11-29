library(tidyverse)
library(drake)
library(PNWColors)


# load files --------------------------------------------------------------
soil_report = read.csv("data/irms_soil_report.csv")
soil_traykey = read.csv("data/irms_soil_traykey.csv") %>% 
  mutate(name = paste0("Tray", tray, "-", position)) %>% dplyr::select(name, sample_no)


# process soil report -----------------------------------------------------
soil_samples = 
  soil_report %>% 
  dplyr::select(name, sample_group, d13C_VPDB) %>% 
  filter(sample_group == "sample") %>% 
  left_join(soil_traykey, by = "name") %>% 
  group_by(sample_no) %>% 
  dplyr::summarise(d13C = round(mean(d13C_VPDB),2),
                   cv_d13C = round(sd(d13C_VPDB)/-d13C,2)) %>% 
  ungroup() %>% 
  rename(core = sample_no) %>% 
  mutate(core = as.character(core))


soil = 
  soil_samples %>% 
  left_join(core_key, by = "core")


soil %>% 
  ggplot(aes(x = treatment, y = d13C, color = type))+
  geom_boxplot()+
  geom_point(size=3, position = position_dodge(width = 0.75))+
  scale_color_manual(values = pnw_palette("Sailboat", 3))+
  labs(title = "δ13C-soil",
       y = "δ13C, ‰")+
  annotate("text", label = "control: soil +  5 mL water", x = 0.7, y = -15, angle = 90)+
  annotate("text", label = "control: \nsoil +  goethite + 5 mL water", x = 1.2, y = -15, angle = 90)+
  annotate("curve", x = 0.7, xend = 0.7, y = -26, yend = -22, curvature = -0.1)+
  annotate("curve", x = 1.3, xend = 1.3, y = -26, yend = -23, curvature = 0.1)+
  theme_kp()+
  NULL

