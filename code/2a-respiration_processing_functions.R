# SOM DESTABILIZATION
# KAIZAD F. PATEL
# Aug 31, 2020

#################### #
#################### #

# 2-respiration_functions

## This script contains functions to process/clean and analyze respiration data.
## These are just the functions, and will be run as part of the processing {drake} plan in a subsequent script.
## You do not need to run this file, it will be sourced in the {drake} plan.

#################### #
#################### #


library(tidyverse)
library(drake)
library(PNWColors)

# respiration functions -- computing ---------------------------------------------------

clean_lgr_output_OLD = function(resp_lgr){
  ## This was the OLD clean_lgr_output function, now defunct
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
    mutate(core = str_replace_all(core, ":","")) %>% 
    dplyr::select(core, D13C_VPDB_CO2) %>% 
    group_by(core) %>% 
    dplyr::summarise(D13C_VPDB_CO2 = mean(D13C_VPDB_CO2))
}
clean_lgr_output = function(resp, core_key){
    resp %>% 
    dplyr::select(lgr_id, c13_co2_permil) %>% 
    rename(ID = lgr_id,
           D13C_VPDB_CO2 = c13_co2_permil) %>% 
    filter(!grepl(" B", ID) & !grepl("ambient", ID)) %>% 
    separate(ID, sep = ": ", into = c("core", "B")) %>% 
    dplyr::select(core, D13C_VPDB_CO2) %>% 
    mutate(D13C_VPDB_CO2 = round(D13C_VPDB_CO2, 2)) %>% 
    # keep only the samples we want
    right_join(core_key %>% dplyr::select(core))
}

# the calculate_RC13 function (below) is rendered redundant by the D13C_VPDB in the `resp_lgr` file
# CO2_636_ppm/CO2_626_ppm gives the 13R value, which can then be used to calculate D13C.
calculate_RC13 = function(lgr_clean){
  lgr_clean %>% 
    group_by(core) %>% 
    dplyr::summarise(CO2_626_ppm = mean(CO2_626_ppm),
                     CO2_636_ppm = mean(CO2_636_ppm),
                     D13C_VPDB_CO2 = mean(D13C_VPDB_CO2)) %>% 
    ungroup() %>% 
    mutate(RC13 = CO2_636_ppm/CO2_626_ppm,
           D13C_calc = ((RC13/0.011237)-1)*1000) %>% 
    dplyr::select(-CO2_626_ppm, -CO2_636_ppm, -D13C_VPDB_CO2) %>% 
    rename(resp_R13C = RC13,
           resp_D13C = D13C_calc)
}

clean_licor_output_x = function(resp_licor){
  resp_licor_temp = 
    resp_licor %>% 
    rename(
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
clean_licor_output = function(resp_licor, core_key){
  ## we calculate the partial pressure of CO2 using the formula:
  ## CO2_post = [(V_eff * CO2_initial) + (V_inj * pCO2)]/[V_eff + V_inj]
  ## therefore, pCO2 = [{V_eff * (CO2_final - CO2_initial)} + (V_inj * CO2_final)] / V_inj
  
  resp_licor_temp = 
    resp_licor %>% 
    rename(
      V_inj = `Inj..Volume`,
      V_eff = `Eff.Volume`,
      CO2_initial = `Initial.CO2`,
      CO2_final = `Final.CO2`,
      #pCO2_ppm1 = `pCO2.raw..ppm.`,
      #CO2_ppm = `Final.CO2`
      ) %>% 
    dplyr::select(ID, V_inj, V_eff, CO2_initial, CO2_final) %>% 
    filter(!grepl("B", ID)) %>% # remove replicates
    separate(ID, sep = ":", into = c("core", "B")) %>% 
    dplyr::select(-B) %>% 
    mutate(pCO2_ppm = ((V_eff * (CO2_final-CO2_initial)) + (V_inj * CO2_final))/ V_inj)
  
  ambient_pCO2 = 
    resp_licor_temp %>% 
    mutate(ambient = grepl("A", core)) %>% 
    filter(ambient) %>% 
    filter(!core %in% "A1") %>% 
    dplyr::summarise(pCO2_ppm = round(mean(pCO2_ppm),3)) %>% pull()
  
  resp_licor_temp %>% 
    mutate(pCO2_ppm = pCO2_ppm - ambient_pCO2) %>% 
    dplyr::select(core, pCO2_ppm) %>% 
    # keep only the samples we want
    right_join(core_key %>% dplyr::select(core))
  
}

calculate_moles_CO2C = function(headspace, licor_clean, core_weights){
  P = 1         # atm
  R = 82.05			# cm3 atm K−1 mol−1
  Temp = 21+273 # K 
  
  # calculate moles of air in the core headspace
  # PV = nRT
  headspace_calc  = 
    headspace %>% 
    mutate(mol_air = (P*headspace_cm3)/(R*Temp))
  
  # calculate moles of CO2 in the core headspace
  # moles CO2/moles air = CO2 conc (ppm)
  licor_clean %>% 
    left_join(dplyr::select(headspace_calc, core, mol_air), by = "core") %>% 
    mutate(mol_CO2C = (pCO2_ppm/1000000) * mol_air,
           umol_CO2C = round(mol_CO2C * 1000000,3)) %>% 
    dplyr::select(core, pCO2_ppm, umol_CO2C) %>% 
    # now convert moles CO2C to grams CO2C
    # and then normalize to total soil weight
    left_join(core_weights %>% dplyr::select(core, od_soil_g), by = "core") %>% 
    mutate(mg_CO2C = umol_CO2C * 12 / 1000,
           CO2C_mg_g = mg_CO2C/od_soil_g) %>% 
    dplyr::select(core, pCO2_ppm, umol_CO2C, CO2C_mg_g)
    
  
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
  gg_resp_old = 
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
  
  gg_resp = 
    respiration %>% 
    ggplot(aes(x = treatment, y = CO2_ppm, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.75))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "CO2 concentration",
         y = "CO2, ppm", 
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
    scale_y_continuous(sec.axis = sec_axis(~. * 13*1000, name = "mg 13C"))+
    labs(title = "umol of 13C-CO2")+
    theme_kp()+
    NULL
  
  list(gg_resp = gg_resp,
       gg_D13C = gg_D13C,
       gg_R13C = gg_R13C,
       gg_13C_umol = gg_13C_umol)  
}

#  

