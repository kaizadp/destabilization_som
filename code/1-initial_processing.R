# SOM DESTABILIZATION
# KAIZAD F. PATEL
# Aug 31, 2020

#################### #
#################### #

# 1-initial_processing

## This script will import the necessary files from Google Drive and save them in the local directory.
## You do not need to run this file each time. Run only once, or to import new/updated files.

#################### #
#################### #

# install.packages("devtools")
# devtools::install_github("tidyverse/googlesheets4")

source("code/0-packages.R")
library(googlesheets4)

# load files from googlesheets -----------------------------------------

## 1. core info ---------------------------------------------------------------
core_key = read_sheet("1k7-Xdav-tRB13cyf3As_MOUk339u3XUuUrPmRwOg7AU") %>% 
  mutate(type = recode(type, "desorption" = "sorbed-C", "priming" = "solution-C")) %>% 
  mutate(substrate_maoc_id = treatment == "2-wetting") %>% 
  filter(substrate_maoc_id) %>% 
  dplyr::select(-substrate_maoc_id) %>% 
  write.csv(COREKEY, row.names = F, na = "")

core_weights = read_sheet("1PR-VvyKcZIYoH3VF8bBWmzkmAnPibf0Fyd_EVrEqvcc")
core_weights %>% 
  mutate(headspace_cm3 = 496.5 - 68.5) %>% 
  write.csv(COREWEIGHTS, row.names = F, na = "")


GRAVMOIST = 0.0261
core_weights_processed = 
  core_weights %>% 
  mutate(headspace_cm3 = 496.5 - 68.5) %>% 
  dplyr::select(core, soil_g, headspace_cm3) %>% 
  rename(fm_soil_g = soil_g) %>% 
  mutate(od_soil_g = fm_soil_g/(GRAVMOIST+1),
         od_soil_g = round(od_soil_g, 1),
         od_soil_g = if_else((core<21 | core>35), od_soil_g+5, od_soil_g)) %>% 
  write.csv("data/processed/core_weights.csv", row.names = F, na = "")


# drying/drying-rewetting soils are drying
# the `drydown` file tracks the weight to see how much water was lost  
core_weights_drydown = read_sheet("1PR-VvyKcZIYoH3VF8bBWmzkmAnPibf0Fyd_EVrEqvcc", sheet = "drydown")



## 2. respiration -------------------------------------------------------------
read_sheet("14tICelWP2cVTHR6K5IyKSvJ_GE-3Ckq8xSlVfJ4KGJU") %>% 
  write.csv("data/respiration.csv", row.names = F)

read_sheet("1nMyKFQRYX5LSpMsl905GRJGjHawKd76E4i0wysXAGNo", sheet = "LGR_Output") %>% 
  write.csv("data/respiration_lgr_output.csv", row.names = F)

read_sheet("1nMyKFQRYX5LSpMsl905GRJGjHawKd76E4i0wysXAGNo", sheet = "LICOR_Output") %>% 
  write.csv("data/respiration_licor_output.csv", row.names = F)

read_sheet("1GJiQ4wKdTOYJ5hDUhkhosYP_ZyRziFSa5gwkvUetFoM") %>% 
  as.data.frame(.) %>% 
  dplyr::select(sample, headspace_vol_mL) %>% 
  rename(core = sample) %>% 
  mutate(core = as.character(core)) %>% 
  write.csv("data/respiration_headspace.csv", row.names = F)


## 3. IRMS AND TOTAL C FILES --------------------------------------------------------------------
### irms soil
read_sheet("1sDUxJV7E5Hrz7p7_xF7QAKj9KzLBl06n_k6bUHRkdJA", sheet = "report") %>% 
  write.csv(IRMS_SOIL_REPORT, row.names = F, na = "")

read_sheet("1sDUxJV7E5Hrz7p7_xF7QAKj9KzLBl06n_k6bUHRkdJA", sheet = "tray_key") %>% 
  write.csv(IRMS_SOIL_TRAYKEY, row.names = F, na = "")

### irms weoc
read_sheet("1W0Gum8I-HTUu61l9_cK2RtttBCq05O1PHFDkMOt6pRE", sheet = "report") %>% 
  mutate(d15N_air = as.numeric(paste0(d15N_air)),
         d13C_VPDB = as.numeric(paste0(d13C_VPDB))) %>% 
  write.csv(IRMS_WEOC_REPORT, row.names = F, na = "")

read_sheet("1W0Gum8I-HTUu61l9_cK2RtttBCq05O1PHFDkMOt6pRE", sheet = "tray_key") %>% 
  write.csv(IRMS_WEOC_TRAYKEY, row.names = F, na = "")

### irms weoc pellets
read_sheet("1ZXgI-r0ZfziWj8kIaYJ4iYPAvii776lOH902wvb7xdk", sheet = "report") %>% 
  mutate(d15N_air = as.numeric(paste0(d15N_air)),
         d13C_VPDB = as.numeric(paste0(d13C_VPDB))) %>% 
  write.csv(IRMS_WEOC_PELLETS_REPORT, row.names = F, na = "")

read_sheet("1pTg_Q2_jsQoYxmXZbUqDxTv4VTENCBemcsMeGDovkAs", sheet = "doc_pellet_map") %>% 
  write.csv(IRMS_WEOC_PELLETS_TRAYKEY, row.names = F, na = "")

### tc reports
read_sheet("1u_WAd8dEymTWItSqIRxYw0QI-5z7m9AfK86FvNSgTrQ", sheet = "report") %>% 
  write.csv(TC_WEOC_REPORT, row.names = F, na = "")

read_sheet("1z506tB8EaWuE98pAOwe5ZTpnXsnTfi6CnaEYz5yXnCQ", sheet = "report") %>% 
  write.csv(TC_SOIL_REPORT, row.names = F, na = "")

read_sheet("1L1whxAlZwgNkvPCS3hVV665o9WWHj9VRIn2-qIfQxSs", sheet = "report") %>% 
  write.csv(TC_WEOC_PELLETS_REPORT, row.names = F, na = "")

### weoc weights
weoc_subsampling = read_sheet("1A8CXukZSxYb3Hpc-XEea2RPvabO2hzvBSimyQT0gu1w", sheet = "Sheet1") %>% 
  mutate(core = as.numeric(as.character(core))) %>% 
  write.csv(WEOC_SUBSAMPLING, row.names = F, na = "")

weoc_capsuleweights = read_sheet("1pTg_Q2_jsQoYxmXZbUqDxTv4VTENCBemcsMeGDovkAs", sheet = "all_weights") %>% 
  mutate(core = as.numeric(as.character(core))) %>% 
  write.csv(WEOC_CAPSULES, row.names = F, na = "")


## 4. picarro respiration glucose response ---------------------------------
# corekey
read_sheet("1Tcj9aKXFAOWWIKWNtnLgzasGM2Bsu5E6Faru3ag-3QY") %>% 
  write.csv("data/picarro_corekey.csv", row.names = F, na = "")

# valvekey
read_sheet("1xbobt7dtvtP0iEkjPknXSD2brracJKvlgiz1N9JGl6Q") %>% 
  write.csv("data/picarro_valvekey.csv", row.names = F, na = "")
