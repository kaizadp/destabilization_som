## calculating Specific Surface Area (SSA) using WP4C

## https://www.metergroup.com/environment/articles/measure-specific-surface-soil-wp4/
## https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2005WR004142
## https://www.agronomy.org/files/publications/nse/pdfs/jnr010/010-01-0073.pdf

source("code/0-packages.R")


ssa_function_test = function(){
## 1. load file from Google Sheets
wp4c_dat = googlesheets4::read_sheet("1JzghBvVxiIwfkmvqucZycqXjmN1xNRSW9M1LLXst6qE") %>% 
  dplyr::filter(is.na(skip)) %>% 
  dplyr::select(-skip)

# set constants
density_w = 1000 # kg/m3
k = -6e-20 #J

ssa_data = wp4c_dat %>% 
  # calculate moisture content
  mutate(moist_g = tray_soil_moist_g - tray_wt_g,
         od_g = tray_soil_OD_g - tray_wt_g,
         water_g = moist_g-od_g,
         water_g_g = round((moist_g-od_g)/od_g,3)) %>% 
  # convert Water Potential units
  # 1 kPa = 1 J/kg, so 10-3 MPa = 1 J/kg,
  # so 1 MPa = 10^3 J/kg
  mutate(J_kg = MPa*1000) %>% 
  # calculate SSA
  # water_g_g = (k/(6 * 3.1416 * density_w * J_kg))^(1/3) * density_w * ssa
  mutate(ssa_m2_kg = (water_g_g/density_w) * (k/(6 * 3.1416 * density_w * J_kg))^(-1/3),
         ssa_m2_g = round(ssa_m2_kg/1000,2))


ssa_data %>% 
  group_by(sample_type, measurement_type, notes) %>% 
  dplyr::summarise(
    mean = mean(ssa_m2_g),
    se = sd(ssa_m2_g)/sqrt(n()),
    cv = (sd(ssa_m2_g)/mean)*100
  )

library(magrittr)
ssa_data %>% 
#  filter(grepl("expt", sample_type)) %$%
  filter(sample_type %in% c("Palouse", "goethite",
                            "expt-Palouse", "expt-P+G", "expt-P+G-slurry")) %$%
  aov(ssa_m2_g ~ sample_type) %>% 
  summary()


ssa_summary = 
  ssa_data %>%
  filter(!is.na(ssa_m2_g)) %>% 
  filter(sample_type %in% c("Palouse", "goethite",
                            "expt-Palouse", "expt-P+G", "expt-P+G-slurry")) %>% 
  dplyr::select(sample_type, measurement_type, water_g_g, ssa_m2_g) %>% 
  group_by(sample_type, measurement_type) %>% 
  dplyr::summarise(moisture = water_g_g*100,
                   ssa_mean = mean(ssa_m2_g),
                   sd = sd(ssa_m2_g),
                   se = sd/sqrt(n()),
                   cv = (sd/ssa_mean)*100) %>% 
  mutate_if(is.numeric, round, 2)

sssa_subset = 
  ssa_data %>% 
  filter(grepl("expt", sample_type) & grepl("mortar", notes) & measurement_type == "f")

sssa_subset %>% 
  ggplot(aes(x = sample_type, y = ssa_m2_g))+
  geom_point()+
  annotate("text", label = "CV = 30.0 %", x = 1.3, y = 80)+
  annotate("text", label = "CV = 7.8 %", x = 2.3, y = 90)+
  
  theme_classic()

}


ssa_calculation = function(){
  ## 1. load file from Google Sheets
  wp4c_dat = googlesheets4::read_sheet("1N8si69IPu84fZ58j_tgnAD_3Iupuyicy-AGAN_ZAwhg") %>% 
    dplyr::filter(is.na(skip)) %>% 
    dplyr::select(-skip)
  
  # set constants
  density_w = 1000 # kg/m3
  k = -6e-20 #J
  
  ssa_data = wp4c_dat %>% 
    # calculate moisture content
    mutate(sample_type = recode(sample_type,
                                "Palouse" = "soil only",
                                "P + Goethite (dry)" = "soil + goethite (dry)",
                                "P + Goethite (paste)" = "soil + goethite (paste)")) %>% 
    mutate(moist_g = tray_soil_moist_g - tray_wt_g,
           od_g = tray_soil_OD_g - tray_wt_g,
           water_g = moist_g-od_g,
           water_g_g = round((moist_g-od_g)/od_g,3)) %>% 
    # convert Water Potential units
    # 1 kPa = 1 J/kg, so 10-3 MPa = 1 J/kg,
    # so 1 MPa = 10^3 J/kg
    mutate(J_kg = MPa*1000) %>% 
    # calculate SSA
    # water_g_g = (k/(6 * 3.1416 * density_w * J_kg))^(1/3) * density_w * ssa
    mutate(ssa_m2_kg = (water_g_g/density_w) * (k/(6 * 3.1416 * density_w * J_kg))^(-1/3),
           ssa_m2_g = round(ssa_m2_kg/1000,2))
  
  
  ssa_summary_cores = 
    ssa_data %>% 
    #filter(ssa_m2_g < 200) %>% 
    group_by(sample_type, rep_ID) %>% 
    dplyr::summarise(
      mean = mean(ssa_m2_g),
      se = sd(ssa_m2_g)/sqrt(n()),
      cv = (sd(ssa_m2_g)/mean)*100,
      mean = round(mean, 1),
      cv = round(cv, 1)
    ) %>% 
    mutate(sample_type = factor(sample_type, levels = c("soil only", "soil + goethite (dry)", "soil + goethite (paste)")))
  
  ssa_data %>% 
    mutate(sample_type = factor(sample_type, 
                                levels = c("soil only", "soil + goethite (dry)", "soil + goethite (paste)"))) %>% 
    ggplot(aes(x = rep_ID, y = ssa_m2_g))+
    geom_point(data = ssa_summary_cores, aes(y = mean), 
               shape = 21, size = 5, stroke = 1, 
               color = "grey50", fill = "yellow", alpha = 0.8) +
    geom_point(size = 2)+
    
    annotate("text", label = "CV:", x = 0.9, y = 300)+
    geom_text(data = ssa_summary_cores, aes(label = paste0(cv, "%"), y = 280))+
    annotate("text", label = "mean:", x = 0.9, y = 360)+
    geom_text(data = ssa_summary_cores, aes(label = mean, y = 340))+
    
    labs(x = "",
         y = expression(bold("SSA, m" ^2 * "g" ^-1)),
         #title = "Specific Surface Area using WP4C",
         caption = "yellow circles = mean value for each sample")+
    scale_y_continuous(limits = c(0, 360),
                       breaks = c(0, 100, 200))+
    facet_wrap(~sample_type, scales = "free_x")+
    theme_kp()+
    theme(panel.grid = element_blank())
  
  ggsave("reports/ssa.png", width = 12, height = 4.5)
  
  
  library(magrittr)
  ssa_summary_cores %$%
    aov(log(cv) ~ sample_type) %>% 
    summary()
  
  a = ssa_summary_cores %$% aov((cv) ~ sample_type)
  hsd = agricolae::HSD.test(a, "sample_type"); hsd
  
  

  plot(ssa_summary_cores$cv)
  
}

