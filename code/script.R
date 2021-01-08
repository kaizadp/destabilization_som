library(tidyverse)


# -------------------------------------------------------------------------


GRAVMOIST = 0.0261

core_weights = read.csv("data/core_weights.csv")

core_weights_processed = 
  core_weights %>% 
  dplyr::select(core, soil_g, headspace_cm3) %>% 
  rename(fm_soil_g = soil_g) %>% 
  mutate(od_soil_g = fm_soil_g/(GRAVMOIST+1),
         od_soil_g = round(od_soil_g, 1))

soil_processed = read.csv("data/processed/soil_processed.csv")
weoc_processed = read.csv("data/processed/weoc_processed.csv")
respiration_processed = read.csv("data/processed/respiration_processed.csv")

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
combined = combine_data(soil_processed, weoc_processed, respiration_processed)




# mass balance ------------------------------------------------------------
soil_carbon_massbalance = 
  core_weights_processed %>% 
  dplyr::select(core, od_soil_g) %>% 
  left_join(dplyr::select(soil_processed, core, totalC_perc), by = "core") %>% 
  left_join(dplyr::select(weoc_processed, core, weoc_mg_g), by = "core") %>% 
  mutate(tc_g = (totalC_perc/100) * od_soil_g,
         weoc_g = (weoc_mg_g * od_soil_g)/1000,
         non_weoc_g = tc_g - weoc_g)

soil_carbon_massbalance %>% 
  #dplyr::select(core, weoc_g, non_weoc_g) %>% 
  #pivot_longer(-core) %>% 
  ggplot(aes(x = core, y = weoc_g))+
  geom_bar(stat = "identity")  


core_key = read.csv(file_in("data/core_key.csv")) %>% mutate(core = as.character(core)) %>% filter(skip != "skip")


soil_13carbon_massbalance = 
  core_key %>% 
  mutate(core = as.integer(core)) %>% 
  dplyr::select(-skip) %>% 
  left_join(core_weights_processed %>% dplyr::select(core, od_soil_g), by = "core") %>% 
  
  left_join(tc_soil_combined %>% rename(tc_d13C_VPDB = d13C_VPDB, tc_R13C = R13C), 
            by = "core") %>% 
  left_join(tc_weoc_combined %>% rename(weoc_d13C_VPDB = d13C_VPDB, weoc_R13C = R13C), 
            by = "core") %>% 
  mutate(core = as.character(core)) %>% 
  
  mutate(tc_g = (totalC_perc/100) * od_soil_g,
         weoc_g = (weoc_mg_g * od_soil_g)/1000,
         non_weoc_g = tc_g - weoc_g) %>% 
  
  mutate(tc_f13C = tc_R13C/(1 + tc_R13C),
         weoc_f13C = weoc_R13C/(1 + weoc_R13C),
         
         tc_13C_g = tc_f13C * tc_g,
         weoc_13C_g = weoc_f13C * weoc_g,
         nonweoc_13C_g = tc_13C_g - weoc_13C_g,
         
         tc_13C_mg = tc_13C_g * 1000,
         weoc_13C_mg = weoc_13C_g * 1000,
         nonweoc_13C_mg = tc_13C_mg - weoc_13C_mg) 


soil_13carbon_massbalance_long = 
  soil_13carbon_massbalance %>% 
  dplyr::select(core, treatment, type, weoc_13C_mg, nonweoc_13C_mg, tc_13C_mg) %>% 
  pivot_longer(-c(core, treatment, type))

soil_13carbon_massbalance_long %>% 
  filter(name == "tc_13C_mg") %>% 
  ggplot(aes(x= as.numeric(core), y = value, fill = treatment))+
  geom_bar(stat = "identity")+
  ylab("13C mg")


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






CONTROL_TC_D13C = 
  control %>% 
  filter(treatment == "1-time-zero") %>% 
  dplyr::summarise(tc_d13C_VPDB = mean(tc_d13C_VPDB)) %>% 
  pull(tc_d13C_VPDB)
  
CONTROL_WEOC_D13C = 
  control %>% 
  filter(treatment == "1-time-zero") %>% 
  dplyr::summarise(weoc_d13C_VPDB = mean(weoc_d13C_VPDB)) %>% 
  pull(weoc_d13C_VPDB)


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
  combined_lgr_licor %>% 
  dplyr::select(core, D13C_calc, CO2_ppm, umol_CO2C, type, treatment) %>% 
  mutate(f_oxalic = (D13C_calc - CONTROL_WEOC_D13C)/(OXALIC_D13C - CONTROL_WEOC_D13C))

resp_fraction %>% 
  ggplot(aes(x = treatment, y = f_oxalic, color = type))+
  geom_point()

summary(aov(D13C_calc ~ treatment,
    data = combined_lgr_licor %>% 
      filter(type == "desorption" & treatment %in% c("2-wetting", "4-drying-rewetting"))
))
  







# misc --------------------------------------------------------------------
C_VPDB = 0.011237

a = 
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
  mutate(R13C_calc = CO2_636_ppm/CO2_626_ppm,
         D13C_calc = ((R13C_calc/C_VPDB) -1) * 1000)


a = 
  licor_clean %>% 
  left_join(dplyr::select(headspace_calc, core, mol_air), by = "core") %>% 
  mutate(mol_CO2C = (CO2_ppm/1000000) * mol_air,
         umol_CO2C = round(mol_CO2C * 1000000,3),
         mg_CO2C = umol_CO2C * 12 * 1000,
         umoles_CO2C = pCO2_ppm * mol_air)


# REDOING RESPIRATION -----------------------------------------------------

licor_temp = 
  resp_licor %>% 
  rename(final_CO2 = `Final.CO2`) %>% 
  dplyr::select(ID, final_CO2) %>% 
  # split ID into core, etc.
  separate(ID, sep = ": ", into = c("core", "misc")) %>% 
  # drop misc == B because reps
  mutate(drop = grepl("B", misc)) %>% 
  filter(!drop) %>% 
  dplyr::select(-drop) %>% 
  filter(core != "A1")

AMBIENT_CO2 = 
  licor_temp %>% 
  filter(grepl("A", core)) %>% 
  summarise(CO2 = mean(final_CO2)) %>% 
  pull(CO2)

licor_temp2 = 
  licor_temp %>% 
  mutate(CO2_ppm_corr = final_CO2 - AMBIENT_CO2) %>% 
  filter(!grepl("A", core))

## PV = nRT
pressure_kPa = 101.325
temp_K = 21+273
R = 8.3145e+3			# cm3 kPa K−1 mol−1
V = 428 # cm3

moles_air = pressure_kPa * V /(R * temp_K)


CO2_moles = 
  licor_temp2 %>% 
  mutate(umoles_CO2 = CO2_ppm_corr * moles_air)


lgr_temp = 
  resp_lgr %>% 
  dplyr::select(ID, D13C_VPDB_CO2) %>% 
  group_by(ID) %>% 
  dplyr::summarise(D13C_VPDB_CO2 = mean(D13C_VPDB_CO2)) %>% 
  drop_na() %>% 
  filter(!grepl("ambient", ID)) %>% 
  filter(!grepl("A4", ID)) %>% 
  filter(!grepl("B", ID)) %>% 
  filter(!grepl("b", ID)) %>% 
  separate(ID, sep = c(" "), into = c("core", "misc")) %>% 
  separate(core, sep = c("_"), into = c("core", "misc2")) %>% 
  separate(core, sep = c(":"), into = c("core", "misc3")) %>% 
  dplyr::select(core, D13C_VPDB_CO2)


C_VPDB = 0.011237 # 13C/12C ratio


combined_resp = 
  left_join(CO2_moles, lgr_temp, by = "core") %>% 
  dplyr::select(-misc) %>% 
  # calculate R and F
  mutate(R13C = ((D13C_VPDB_CO2/1000)+1) * C_VPDB,
         F13C = R13C/(1+R13C),
         umol_13C = F13C * umoles_CO2,
         umol_12C = (1-F13C) * umoles_CO2,
         ug_13C = umol_13C * 13,
         ug_12C = umol_12C * 12) %>% 
  left_join(core_weights_processed %>% dplyr::select(core, od_soil_g) %>% mutate(core = as.character(core)), by = "core") %>% 
  mutate(ug_13C_g = ug_13C / od_soil_g,
         ug_12C_g = ug_12C / od_soil_g,
         ug_CO2C_g = ug_13C_g + ug_12C_g) %>% 
  left_join(core_key) 

combined_resp %>% 
  ggplot(aes(x = treatment, y = ug_CO2C_g))+
  geom_point()

