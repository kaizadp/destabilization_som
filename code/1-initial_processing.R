# install.packages("devtools")
# devtools::install_github("tidyverse/googlesheets4")

library(googlesheets4)


# load files from googlesheets -----------------------------------------
core_key = read_sheet("1k7-Xdav-tRB13cyf3As_MOUk339u3XUuUrPmRwOg7AU") %>% 
  write.csv("data/core_key.csv", row.names = F)

core_weights = read_sheet("1PR-VvyKcZIYoH3VF8bBWmzkmAnPibf0Fyd_EVrEqvcc")


# drying/drying-rewetting soils are drying
# the `drydown` file tracks the weight to see how much water was lost  
core_weights_drydown = read_sheet("1PR-VvyKcZIYoH3VF8bBWmzkmAnPibf0Fyd_EVrEqvcc",
                                  sheet = "drydown")



# respiration -------------------------------------------------------------
read_sheet("1GWK1c5ruKiZmJ6mjZzCk-gBw6dL0eJ1OxT0OKHBsVbo") %>% 
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


# irms --------------------------------------------------------------------
read_sheet("1sDUxJV7E5Hrz7p7_xF7QAKj9KzLBl06n_k6bUHRkdJA", sheet = "report") %>% 
  write.csv("data/irms_soil_report.csv", row.names = F, na = "")
read_sheet("1sDUxJV7E5Hrz7p7_xF7QAKj9KzLBl06n_k6bUHRkdJA", sheet = "tray_key") %>% 
  write.csv("data/irms_soil_traykey.csv", row.names = F, na = "")

