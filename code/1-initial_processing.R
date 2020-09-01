# install.packages("devtools")
# devtools::install_github("tidyverse/googlesheets4")

library(googlesheets4)


# load files from googlesheets -----------------------------------------
core_key = read_sheet("1k7-Xdav-tRB13cyf3As_MOUk339u3XUuUrPmRwOg7AU")

core_weights = read_sheet("1PR-VvyKcZIYoH3VF8bBWmzkmAnPibf0Fyd_EVrEqvcc")


# drying/drying-rewetting soils are drying
# the `drydown` file tracks the weight to see how much water was lost  
core_weights_drydown = read_sheet("1PR-VvyKcZIYoH3VF8bBWmzkmAnPibf0Fyd_EVrEqvcc",
                                  sheet = "drydown")


