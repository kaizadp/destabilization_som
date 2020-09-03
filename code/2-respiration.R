

library(tidyverse)
library(drake)
library(PNWColors)


respiration_plan = 
  drake_plan(
  # load file ---------------------------------------------------------------
  resp_data = read.csv(file_in("data/respiration.csv")),
  core_key = read.csv(file_in("data/core_key.csv")) %>% 
    mutate(core = as.character(core)),
  theme_set(theme_bw()),
  
  
  # clean file --------------------------------------------------------------
  respiration = 
    resp_data %>% 
    filter(is.na(rep)|rep!="B") %>% 
    separate(lgr_id, sep = ": ", into = c("core", "treatment")) %>% 
    select(-treatment) %>% 
    left_join(select(core_key, core, type, treatment)) %>% 
    filter(!is.na(treatment)),
  
  
  
  # plots -------------------------------------------------------------------
  
  gg_13c = 
    respiration %>% 
    ggplot(aes(x = treatment, y = c13_co2_permil, color = type))+
    geom_point(size=3, position = position_dodge(width = 0.5))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    NULL,
  
  gg_co2 = 
    respiration %>% 
    ggplot(aes(x = treatment, y = co2_ppm, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.7))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    NULL,
  
  
  # report ------------------------------------------------------------------
  
  report = rmarkdown::render(
    knitr_in("reports/respiration_drake_report.Rmd"),
    output_format = rmarkdown::github_document())  
  
  # -------------------------------------------------------------------------
)

make(respiration_plan)
