source("code/0-packages.R")
library(outliers)
library(agricolae)

# combine and process files -----------------------------------------------------------

combine_data = function(soil, weoc, respiration, core_key, core_weights){
  ## this function will combine all three fractions of data
  ## first, we clean each file and make sure the columns are compatible
  ## then, combine
  ## then, add core key and core weights

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
  
  combined = 
    bind_rows(soil_new, weoc_new, respiration_new) %>% 
    mutate(d13C_VPDB = round(d13C_VPDB,3)) %>% 
    filter(core != 40) %>% 
    drop_na()
  
  core_key %>% 
    dplyr::select(-skip) %>% 
    left_join(core_weights%>% dplyr::select(core, od_soil_g) %>% mutate(core = as.character(core)), by = "core") %>% 
    left_join(combined, by = "core")
}

remove_outliers = function(combined_data){
  fit_dixon_d13C = function(dat){
    dixon.test(dat %>% pull(d13C_VPDB), opposite = FALSE, two.sided = FALSE) %>% 
      broom::tidy() %>% 
      filter(`p.value` <= 0.10) %>% 
      mutate(outlier = case_when(grepl("highest", alternative) ~ "highest", 
                                 grepl("lowest", alternative) ~ "lowest")) %>% 
      dplyr::select(outlier)
  }
  
  c13_outliers_combined = 
    combined_data %>%  
    group_by(type, treatment, fraction) %>% 
    do(fit_dixon_d13C(.))
  
  combined_data %>% 
    ungroup %>% 
    left_join(c13_outliers_combined) %>% 
    group_by(type, treatment, fraction) %>% 
    mutate(remove = case_when((outlier == "highest" & d13C_VPDB == max(d13C_VPDB)) | 
                                (outlier == "lowest" & d13C_VPDB == min(d13C_VPDB)) ~ "skip")) %>% 
    filter(is.na(remove)) %>% 
    dplyr::select(-remove, -outlier)
}

calculate_indices = function(combined_data_outliers){
  R13C_VPDB = 0.011237
  
  combined_data_outliers %>% 
    mutate(R13C = ((d13C_VPDB/1000) + 1) * R13C_VPDB,
           F13C = R13C/(1+R13C),
           R13C = round(R13C, 4),
           F13C = round(F13C, 4),
           C13_mg_g = F13C * C_mg_g)
  # convert delta into R and F
  
}

#
# graphs ------------------------------------------------------------------

make_graphs_prelim = function(combined_data_processed){
  ### Respiration ----
  gg_resp_d13c = 
    combined_data_processed %>% 
    filter(fraction == "respiration") %>% 
    ggplot(aes(x = treatment, y = d13C_VPDB, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.75))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "δ13C-CO2",
         y = "δ13C, ‰")+
    theme_kp()+
    NULL
  
  gg_resp_c = 
    combined_data_processed %>% 
    filter(fraction == "respiration") %>% 
    ggplot(aes(x = treatment, y = C_mg_g*1000, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.75))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "CO2-C evolved",
         y = "CO2-C, μg/g", 
         caption = "blank-corrected with ambient")+
    theme_kp()+
    NULL
  
  gg_resp_13c = 
    combined_data_processed %>% 
    filter(fraction == "respiration") %>% 
    ggplot(aes(x = treatment, y = C13_mg_g*1000, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.75))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "mg of 13C-CO2 evolved",
         y = "CO2-13C, μg/g")+
    theme_kp()+
    NULL
  
  ### Soil and WEOC ----
  
  gg_soil_d13c = 
    combined_data_processed %>% 
    filter(fraction != "respiration") %>% 
    ggplot(aes(x = treatment, y = d13C_VPDB, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.75))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "δ13C",
         y = "δ13C, ‰")+
    facet_grid(fraction ~., scales = "free_y")+
    theme_kp()+
    NULL
  
  gg_soil_c = 
    combined_data_processed %>% 
    filter(fraction != "respiration") %>% 
    ggplot(aes(x = treatment, y = C_mg_g, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.75))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    facet_grid(fraction ~., scales = "free_y")+
    labs(title = "C content")+
    theme_kp()+
    NULL
  
  gg_soil_13c = 
    combined_data_processed %>% 
    filter(fraction != "respiration") %>% 
    ggplot(aes(x = treatment, y = C13_mg_g*1000, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.75))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "mg of 13C",
         y = "13C, μg/g")+
    facet_grid(fraction ~., scales = "free_y")+
    theme_kp()+
    NULL
}

make_graphs_desorption = function(combined_data_processed){
  # first, calculate mean values for control soils. use this in the graphs
  control_summary = 
    combined_data_processed %>% 
    filter(type %in% c("control")) %>% 
    group_by(fraction) %>% 
    dplyr::summarise(d13C_VPDB = mean(d13C_VPDB),
                     C_mg_g = mean(C_mg_g),
                     C13_mg_g = mean(C13_mg_g)) %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
  
  # prepare graph labels
  make_desorption_labels = function(combined_data_processed){
    
    # a. set y-axis values
    desorption_label_y = tribble(
      ~type, ~fraction, ~name, ~y,
      "desorption", "respiration", "d13C_VPDB", 1000,
      "desorption", "weoc", "d13C_VPDB", 30,
      "desorption", "soil", "d13C_VPDB", -10,
      
      "desorption", "respiration", "C_mg_g", 0.0025,
      "desorption", "weoc", "C_mg_g", 0.14,
      "desorption", "soil", "C_mg_g", 33,    
      
      "desorption", "respiration", "C13_mg_g", 0.05,
      "desorption", "weoc", "C13_mg_g", 1.4,
      "desorption", "soil", "C13_mg_g", 345
    )
    
    # b. then, calculate HSD for labels 
    compute_hsd_desorption = function(combined_data_processed){
      fit_hsd = function(dat){
        a = aov(value ~ treatment, data = dat)
        h = HSD.test(a, "treatment")
        h$groups %>% mutate(treatment = row.names(.)) %>% 
          rename(label = groups) %>%  
          dplyr::select(treatment, label)
      }
      
      combined_data_processed %>% 
        filter(type == "desorption") %>% 
        dplyr::select(core, type, treatment, fraction, d13C_VPDB, C_mg_g, C13_mg_g) %>% 
        pivot_longer(-c(core, type, treatment, fraction)) %>% 
        group_by(type, fraction, name) %>% 
        do(fit_hsd(.)) # %>% pivot_wider(names_from = "name", values_from = "label")
    }
    desorption_hsd_label = compute_hsd_desorption(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
    
    # c. then, merge the labels with the y
    desorption_hsd_label %>% left_join(desorption_label_y, by = c("type", "fraction", "name")) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
  }
  desorption_labels = make_desorption_labels(combined_data_processed)
  
  gg_desorp_d13c = 
    combined_data_processed %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
    filter(type %in% c("desorption")) %>% 
    ggplot(aes(x = treatment, y = d13C_VPDB, color = fraction))+
    geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
    geom_point(size = 3, show.legend = FALSE) +
    geom_text(data = desorption_labels %>% filter(name == "d13C_VPDB"), aes(label = label, y = y), color = "black")+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "δ13C enrichment in each fraction",
         caption = "dashed line = avg of control samples")+
    facet_grid(fraction~., scales = "free_y")+
    theme_kp()+
    NULL
  
  gg_desorp_c = 
    combined_data_processed %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
    filter(type %in% c("desorption")) %>% 
    ggplot(aes(x = treatment, y = C_mg_g, color = fraction))+
    geom_hline(data = control_summary, aes(yintercept = C_mg_g), linetype = "dashed", color = "grey30")+
    geom_point(size = 3, show.legend = FALSE) +
    geom_text(data = desorption_labels %>% filter(name == "C_mg_g"), aes(label = label, y = y), color = "black")+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "total carbon in each fraction (mg)",
         caption = "dashed line = avg of control samples")+
    facet_grid(fraction~., scales = "free_y")+
    theme_kp()+
    NULL
  
  gg_desorp_c13 = 
    combined_data_processed %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
    filter(type %in% c("desorption")) %>% 
    ggplot(aes(x = treatment, y = C13_mg_g*1000, color = fraction, shape = type))+
    geom_hline(data = control_summary, aes(yintercept = C13_mg_g*1000), linetype = "dashed", color = "grey30")+
    geom_point(size = 3, show.legend = FALSE) +
    geom_text(data = desorption_labels %>% filter(name == "C13_mg_g"), aes(label = label, y = y), color = "black")+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "13C in each fraction (mg)",
         caption = "dashed line = avg of control samples",
         y = "C13, μg/g")+
    facet_grid(fraction~., scales = "free_y")+
    theme_kp()+
    NULL
  
  list(gg_desorp_d13c = gg_desorp_d13c,
       gg_desorp_c = gg_desorp_c,
       gg_desorp_c13 = gg_desorp_c13)
}

make_graphs_priming = function(combined_data_processed){
  
  gg_priming_d13c = 
    combined_data_processed %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
    filter(type %in% c("priming")) %>% 
    ggplot(aes(x = treatment, y = d13C_VPDB, color = fraction))+
    #geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
    geom_point(size = 3, show.legend = FALSE) +
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "δ13C enrichment in each fraction"
    )+
    facet_grid(fraction~., scales = "free_y")+
    theme_kp()+
    NULL
  
  
  gg_priming_c13 = 
    combined_data_processed %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
    filter(type %in% c("priming")) %>% 
    ggplot(aes(x = treatment, y = C13_mg_g*1000, color = fraction))+
    #geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
    geom_point(size = 3, show.legend = FALSE) +
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "C13 content",
         y = "C13 (μg/g)")+
    facet_grid(fraction~., scales = "free_y")+
    theme_kp()+
    NULL
  
  list(gg_priming_d13c = gg_priming_d13c,
       gg_priming_c13 = gg_priming_c13)
  
}

calculate_mass_balance = function(){}




# mixing model for respiration --------------------------------------------

compute_mixing_model = function(){
OXALIC_D13C = 15098.6 # permille

resp_fraction = 
  combined_data_key %>% 
  filter(fraction == "respiration") %>% 
  mutate(f_oxalic = (d13C_VPDB - CONTROL_WEOC_D13C)/(OXALIC_D13C - CONTROL_WEOC_D13C))

resp_fraction %>% 
  filter(type == "priming") %>% 
  ggplot(aes(x = treatment, y = f_oxalic * 100, color = type))+
  geom_point()+
  labs(title = "% contribution of oxalic-13C to respiration")

summary(aov(D13C_calc ~ treatment,
            data = combined_lgr_licor %>% 
              filter(type == "desorption" & treatment %in% c("2-wetting", "4-drying-rewetting"))
))

}


# -------------------------------------------------------------------------
# LABELS -- IGNORE -------------------------------------------------------------------------



##    combined_label = tribble(
##      ~type, ~treatment, ~fraction, ~d13C_VPDB, ~C_mg_g, ~label,
##      "desorption", "1-time-zero", "respiration", 100, NA, "a",
##      "desorption", "2-wetting", "respiration", 100, NA, "ab",
##      "desorption", "3-drying", "respiration", 100, NA, "c",
##      "desorption", "4-drying-rewetting", "respiration", 100, NA, "b",
##      
##      "desorption", "1-time-zero", "respiration", NA, 0.03, "a",
##      "desorption", "2-wetting", "respiration", NA, 0.03, "b",
##      "desorption", "3-drying", "respiration", NA, 0.03, "c",
##      "desorption", "4-drying-rewetting", "respiration", NA, 0.03, "a"
##      
##      #  "desorption", "1-time-zero", "weoc", 
##      #  "desorption", "2-wetting", "weoc", 
##      #  "desorption", "3-drying", "weoc", 
##      #  "desorption", "4-drying-rewetting", "weoc", 
##      
##      
##      
##    ) %>% 
##      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))

# -------------------------------------------------------------------------

# CONTROL -- IGNORE -----------------------------------------------------------------


##    ## what is the isotope composition of control soil?
##    core_key = read.csv(file_in("data/core_key.csv")) %>% mutate(core = as.character(core)) %>% filter(skip != "skip")
##    
##    control = 
##      core_weights_processed %>% 
##      dplyr::select(core, od_soil_g) %>% 
##      left_join(tc_soil_combined %>% rename(tc_d13C_VPDB = d13C_VPDB, tc_R13C = R13C), 
##                by = "core") %>% 
##      left_join(tc_weoc_combined %>% rename(weoc_d13C_VPDB = d13C_VPDB, weoc_R13C = R13C), 
##                by = "core") %>% 
##      mutate(core = as.character(core)) %>% 
##      left_join(core_key, by = "core") %>% 
##      filter(type == "control")
##    
##    
##    control = 
##      combined_data_key %>% 
##      filter(type == "control")
##    
##    CONTROL_TC_D13C = 
##      control %>% 
##      filter(treatment == "1-time-zero" & fraction == "soil") %>% 
##      dplyr::summarise(d13C_VPDB = mean(d13C_VPDB)) %>% 
##      pull(d13C_VPDB)
##    
##    CONTROL_WEOC_D13C = 
##      control %>% 
##      filter(treatment == "1-time-zero" & fraction == "weoc") %>% 
##      dplyr::summarise(d13C_VPDB = mean(d13C_VPDB)) %>% 
##      pull(d13C_VPDB)
##    
##    
##            a = 
##              licor_clean %>% 
##              left_join(dplyr::select(headspace_calc, core, mmol_air), by = "core") %>% 
##              mutate(mmol_CO2C = CO2_ppm * mmol_air/1000000,
##                     umol_CO2C = round(mmol_CO2C * 1000,3)) %>% 
##              dplyr::select(core, CO2_ppm, umol_CO2C) %>% 
##              left_join(core_key, by = "core") 
##            
##            a %>% 
##              ggplot(aes(x = treatment, y = CO2_ppm, color = type))+
##              geom_point(position = position_dodge(width = 0.4))
##            
##            summary(
##              aov(CO2_ppm ~ type, 
##                  data = 
##                    a %>% 
##                    filter(type %in% c("control", "priming") &
##                             treatment == "1-time-zero" &
##                             core != 40))
##            )
##            
##            a_priming = 
##              a %>% 
##              filter(type %in% "priming" & treatment == "1-time-zero")
##            
##            dixon.test(a_priming %>% pull(CO2_ppm),
##                       type = 0, opposite = FALSE, two.sided = TRUE) %>% 
##              summary()
##            
##            
##          


