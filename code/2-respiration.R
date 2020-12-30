

library(tidyverse)
library(drake)
library(PNWColors)

# respiration functions -- compute ---------------------------------------------------

clean_lgr_output = function(resp_lgr){
  resp_lgr %>% 
    filter(!is.na(ID)) %>% 
    rename(CO2_626_ppm = `X.CO2_626._ppm`,
           CO2_636_ppm = `X.CO2_636._ppm`) %>% 
    dplyr::select(Date, Time, ID, CO2_626_ppm, CO2_636_ppm, D13C_VPDB_CO2) %>% 
    separate(ID, sep = " ", into = c("A", "B", "C")) %>% 
    filter(!C %in% c("B", "b")) %>% 
    dplyr::select(-B, -C) %>% 
    separate(A, sep = "_", into = c("core", "B")) %>% 
    dplyr::select(-B) %>% 
    mutate(core = str_replace_all(core, ":",""))
}
calculate_RC13 = function(lgr_clean){
  lgr_clean %>% 
    group_by(core) %>% 
    dplyr::summarise(CO2_626_ppm = mean(CO2_626_ppm),
                     CO2_636_ppm = mean(CO2_636_ppm),
                     D13C_VPDB_CO2 = mean(D13C_VPDB_CO2)) %>% 
    ungroup() %>% 
    mutate(RC13 = CO2_636_ppm/CO2_626_ppm,
           D13C_calc = ((RC13/0.011237)-1)*1000) %>% 
    dplyr::select(-CO2_626_ppm, -CO2_636_ppm, -D13C_VPDB_CO2)
}

clean_licor_output = function(resp_licor){
  resp_licor_temp = 
    resp_licor %>% 
    rename(CO2_ppm = `Final.CO2`,
           pCO2_ppm = `pCO2.raw..ppm.`,
           CO2_ppm = `Final.CO2`) %>% 
    dplyr::select(ID, CO2_ppm, pCO2_ppm) %>% 
    filter(!grepl("B", ID)) %>% # remove replicates
    separate(ID, sep = ":", into = c("core", "B")) %>% 
    dplyr::select(-B)
  
  ambient_pCO2 = 
    resp_licor_temp %>% 
    mutate(ambient = grepl("A", core)) %>% 
    filter(ambient) %>% 
    filter(!core %in% "A1") %>% 
    dplyr::summarise(pCO2_ppm = round(mean(pCO2_ppm),3)) %>% pull()
  
  ambient = 
    resp_licor_temp %>% 
    mutate(ambient = grepl("A", core)) %>% 
    filter(ambient) %>% 
    filter(!core %in% "A1") %>% 
    dplyr::summarise(CO2_ppm = round(mean(CO2_ppm),3)) %>% pull()
  
  resp_licor_temp %>% 
    mutate(pCO2_ppm = pCO2_ppm - ambient_pCO2,
           CO2_ppm = CO2_ppm - ambient)
    
}
calculate_moles_CO2C = function(headspace, licor_clean){
  P = 1         # atm
  R = 82.05			# cm3 atm K−1 mol−1
  Temp = 21+273 # K 
  
  headspace_calc  = 
    headspace %>% 
    mutate(mmol_air = 1000*(P*headspace_cm3)/(R*Temp))
  
  licor_clean %>% 
    left_join(dplyr::select(headspace_calc, core, mmol_air), by = "core") %>% 
    mutate(mmol_CO2C = CO2_ppm * mmol_air/1000000,
           umol_CO2C = round(mmol_CO2C * 1000,3)) %>% 
    dplyr::select(core, CO2_ppm, umol_CO2C)
  
  #list(licor_clean2 = licor_clean2)
}

compute_priming = function(dat){
  # formula taken from Bastida et al. Nat. Comms. https://doi.org/10.1038/s41467-019-11472-7 
  
  resp_mean = 
    dat %>% 
    group_by(type, treatment) %>% 
    dplyr::summarise(umol_12CO2C = round(mean(umol_12CO2C),3),
                     umol_13CO2C = round(mean(umol_13CO2C),3),
                     umol_CO2C = round(mean(umol_CO2C),3)) %>% 
    ungroup()
  
  
  control_CO2C_umol = resp_mean %>% filter(type %in% "control") %>% pull(umol_CO2C)
  substrate_CO2C_umol = resp_mean %>% filter(!type %in% "control") %>% pull(umol_CO2C)
  substrate_13CO2C_umol = resp_mean %>% filter(!type %in% "control") %>% pull(umol_13CO2C)
  
  priming = (substrate_CO2C_umol - substrate_13CO2C_umol) - control_CO2C_umol
  
  tibble(priming_umolC = priming)
}
make_priming_table = function(combined_lgr_licor){
  priming_desorption = 
    combined_lgr_licor %>% 
    filter(!type %in% "priming") %>% 
    group_by(treatment) %>% 
    do(compute_priming(.)) %>% 
    rename(desorption_umolC = priming_umolC)
  
  priming_priming = 
    combined_lgr_licor %>% 
    filter(!type %in% "desorption") %>% 
    group_by(treatment) %>% 
    do(compute_priming(.)) %>% 
    rename(priming_umolC = priming_umolC)
  
  priming_table = 
    priming_desorption %>% left_join(priming_priming)
  
  list(priming_table = priming_table)
}


# respiration functions -- plotting ---------------------------------------
theme_kp <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=1.5, fill = NA),
          
          plot.title = element_text(hjust = 0.05, size = 14),
          axis.text = element_text(size = 10, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

plot_respiration = function(respiration){
  gg_resp = 
    respiration %>% 
    ggplot(aes(x = treatment, y = pCO2_ppm, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.75))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "partial pressure of CO2",
         y = "pCO2, ppm", 
         caption = "blank-corrected with ambient")+
    theme_kp()+
    NULL
  
  gg_D13C = 
    respiration %>% 
    ggplot(aes(x = treatment, y = D13C_calc, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.75))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "Δ13C-CO2",
         y = "Δ13C, ‰")+
    theme_kp()+
    NULL
  
  gg_R13C = 
    respiration %>% 
    ggplot(aes(x = treatment, y = RC13, color = type))+
    geom_point(size=3, position = position_dodge(width = 0.5))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "R C13/C12 in CO2")+
    theme_kp()+
    NULL 
  
  gg_13C_umol = 
    respiration %>% 
    ggplot(aes(x = treatment, y = umol_13CO2C, color = type))+
    geom_point(size=3, position = position_dodge(width = 0.5))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "umol of 13C-CO2")+
    theme_kp()+
    NULL
  
  list(gg_resp = gg_resp,
       gg_D13C = gg_D13C,
       gg_R13C = gg_R13C,
       gg_13C_umol = gg_13C_umol)  
}

#  

# respiration processing plan ---------------------------------------------------------------------
respiration_processing_plan = 
  drake_plan(
    core_key = read.csv(file_in("data/core_key.csv")) %>% mutate(core = as.character(core)) %>% filter(skip != "skip"),
    #headspace = read.csv("data/respiration_headspace.csv"),
    headspace = read.csv("data/core_weights.csv") %>% dplyr::select(core, headspace_cm3) %>% 
      mutate(core = as.character(core)),
    resp_lgr = read.csv("data/respiration_lgr_output.csv"),
    resp_licor = read.csv("data/respiration_licor_output.csv"),
    
    lgr_clean = clean_lgr_output(resp_lgr),
    lgr_calc = calculate_RC13(lgr_clean),
    
    licor_clean = clean_licor_output(resp_licor),
    licor_moles = calculate_moles_CO2C(headspace, licor_clean),
    
    combined_lgr_licor = 
      lgr_calc %>% 
      left_join(licor_moles, by = "core") %>% 
      mutate(umol_12CO2C = umol_CO2C/(RC13 + 1),
             umol_13CO2C = umol_CO2C - umol_12CO2C) %>% 
      left_join(select(core_key, core, type, treatment), by = "core") %>% 
      filter(!is.na(treatment)),
    
    respiration_output = combined_lgr_licor %>% 
      write.csv("data/processed/respiration.csv", row.names = F)
  )

make(respiration_processing_plan)



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

