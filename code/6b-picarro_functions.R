


library(drake)
pkgconfig::set_config("drake::strings_in_dots" = "literals")
library(lubridate)


read_core_key <- function(filename) {
  readxl::read_excel("data/Core_key.xlsx") %>%
    dplyr::select(Core, soil_type, treatment, trt, Core_assignment, Moisture, skip)
}
#ca <- read_core_key("data/Core_key.xlsx")

read_core_dryweights <- function(filename, sheet) {
  read_excel(filename, sheet = sheet) %>% 
    dplyr::select(Core, EmptyWt_g, DryWt_g, Soil_g, Carbon_g)
}
#dry <- read_core_dryweights("data/Core_weights.xlsx", sheet = "initial")

read_core_masses <- function(filename, sheet, core_key, core_dry_weights) {
  readxl::read_excel(filename, sheet = sheet) %>% 
    filter(!is.na(Site), Site != "AMB", Core != "0") %>% # remove unnecessary crap
    left_join(core_key, by = "Core") %>% 
    left_join(core_dry_weights, by = "Core") %>% 
    filter(is.na(skip)) %>% # exclude the rows as needed
    dplyr::select(Core, Start_datetime, Stop_datetime, Seq.Program, Valve,
                  Core_assignment, EmptyWt_g, DryWt_g, Mass_g, Carbon_g, Moisture) %>% 
    dplyr::mutate(Start_datetime = mdy_hm(Start_datetime, tz = "America/Los_Angeles"),
                  Stop_datetime = mdy_hm(Stop_datetime, tz = "America/Los_Angeles"),
                  # calculate moisture content for each core
                  DryWt_g = round(DryWt_g,2),
                  MoistWt_g = Mass_g - EmptyWt_g,
                  Water_g = MoistWt_g - DryWt_g,
                  Moisture_perc = round(((Water_g / DryWt_g) * 100), 2))
}
#mass <- read_core_masses("data/Core_weights.xlsx", sheet = "Mass_tracking", ca, dry)



read_core_key = function(COREKEY){
  read.csv(COREKEY, na.strings = "") %>% 
  mutate(dry_soil_g = (fmsoil_g/(0.04+1)) + goethite_g,
         dry_soil_g = round(dry_soil_g, 1)) %>% 
  dplyr::select(Core, goethite, treatment_mgC, dry_soil_g)
}

read_valve_key = function(VALVEKEY, core_key){
  read.csv(VALVEKEY, na.strings = "") %>% 
    filter(!Core %in% c("blank", "AMBIENT") & !is.na(Core)) %>% 
    mutate(Start_datetime = ymd_hms(Start_datetime),
           Stop_datetime = ymd_hms(Stop_datetime)) %>% 
    dplyr::select(Start_datetime, Stop_datetime, Core, Valve, `Headspace..cm`) %>% 
    left_join(core_key, by = "Core")
}
