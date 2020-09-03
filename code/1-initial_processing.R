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
