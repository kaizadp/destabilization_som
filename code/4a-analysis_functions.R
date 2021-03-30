source("code/0-packages.R")
library(outliers)
library(agricolae)

# combine and process files -----------------------------------------------------------

combine_data = function(soil, weoc, weoc_pellet, respiration, core_key, core_weights){
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
  
  weoc_pellet_new = 
    weoc_pellet %>% 
    rename(C_mg_g = totalC_mg_g) %>% 
    mutate(fraction = "weoc_pellet") %>% 
    dplyr::select(core, fraction, C_mg_g, d13C_VPDB) %>% 
    mutate(core = as.character(core))
  
  respiration_new = 
    respiration %>% 
    rename(C_mg_g = CO2C_mg_g,
           d13C_VPDB = D13C_VPDB_CO2) %>% 
    mutate(fraction = "respiration") %>% 
    dplyr::select(core, fraction, C_mg_g, d13C_VPDB)
  
  combined = 
    bind_rows(soil_new, weoc_new, weoc_pellet_new, respiration_new) %>% 
    mutate(d13C_VPDB = round(d13C_VPDB,3)) %>% 
    filter(core != 40) %>% 
    drop_na()
  
  core_key %>% 
    dplyr::select(-skip) %>% 
    mutate(core = as.character(core)) %>% 
    left_join(core_weights%>% dplyr::select(core, od_soil_g) %>% mutate(core = as.character(core)), by = "core") %>% 
    left_join(combined, by = "core") %>% 
    filter(!is.na(fraction))
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
    filter(fraction == "weoc_pellet") %>% 
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
  combined_data_processed = 
    combined_data_processed %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
  
  
  gg_prelim_d13c = 
    combined_data_processed %>% 
    #filter(fraction == "respiration") %>% 
    ggplot(aes(x = treatment, y = d13C_VPDB, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.75))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "δ13C",
         y = "δ13C, ‰")+
    theme_kp()+
    facet_grid(fraction~., scales = "free_y")+
    NULL
  
  gg_prelim_c =
    combined_data_processed %>% 
    #filter(fraction == "respiration") %>% 
    ggplot(aes(x = treatment, y = C_mg_g*1000, color = type))+
    geom_boxplot()+
    geom_point(size=3, position = position_dodge(width = 0.75))+
    scale_color_manual(values = pnw_palette("Sailboat", 3))+
    labs(title = "total C (12C + 13C)",
         y = "total C, μg/g", 
         caption = "resp is blank-corrected with ambient")+
    theme_kp()+
    facet_grid(fraction~., scales = "free_y")+
    NULL
  
  ##  gg_resp_13c = 
  ##    combined_data_processed %>% 
  ##    filter(fraction == "respiration") %>% 
  ##    ggplot(aes(x = treatment, y = C13_mg_g*1000, color = type))+
  ##    geom_boxplot()+
  ##    geom_point(size=3, position = position_dodge(width = 0.75))+
  ##    scale_color_manual(values = pnw_palette("Sailboat", 3))+
  ##    labs(title = "mg of 13C-CO2 evolved",
  ##         y = "CO2-13C, μg/g")+
  ##    theme_kp()+
  ##    NULL
  
  list(gg_prelim_d13c = gg_prelim_d13c,
       gg_prelim_c = gg_prelim_c)
}

make_graphs_desorption2 = function(combined_data_processed){
  ## UPDATE 2021-01-15: THIS IS NOW DEFUNCT
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
      "sorbed-C", "respiration", "d13C_VPDB", 1000,
      "sorbed-C", "weoc", "d13C_VPDB", 30,
      "sorbed-C", "soil", "d13C_VPDB", -10,
      
      "sorbed-C", "respiration", "C_mg_g", 0.0025,
      "sorbed-C", "weoc", "C_mg_g", 0.14,
      "sorbed-C", "soil", "C_mg_g", 33,    
      
      "sorbed-C", "respiration", "C13_mg_g", 0.05,
      "sorbed-C", "weoc", "C13_mg_g", 1.4,
      "sorbed-C", "soil", "C13_mg_g", 345
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
        filter(type == "sorbed-C") %>% 
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
    filter(type %in% c("sorbed-C")) %>% 
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
    filter(type %in% c("sorbed-C")) %>% 
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
    filter(type %in% c("sorbed-C")) %>% 
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

make_graphs_priming2 = function(combined_data_processed){
  ## UPDATE 2021-01-15: THIS IS NOW DEFUNCT
  
  gg_priming_d13c = 
    combined_data_processed %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
    filter(type %in% c("solution-C")) %>% 
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
    filter(type %in% c("solution-C")) %>% 
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

make_graphs_d13c = function(combined_data_processed){
  ## UPDATE 2021-01-29: THIS IS NOW DEFUNCT
  
  # first, calculate mean values for control soils. use this in the graphs
  control_summary = 
    combined_data_processed %>% 
    filter(type %in% c("control")) %>% 
    group_by(fraction) %>% 
    dplyr::summarise(d13C_VPDB = mean(d13C_VPDB),
                     C_mg_g = mean(C_mg_g),
                     C13_mg_g = mean(C13_mg_g)) %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
  
  # second, prepare graph labels
  make_hsd_labels = function(combined_data_processed){
    
    # a. set y-axis values
    desorption_label_y = tribble(
      ~analysis, ~fraction, ~name, ~y,
      "HSD",  "respiration", "d13C_VPDB", 2100,
      "HSD",  "weoc", "d13C_VPDB", 30,
      "HSD",  "soil", "d13C_VPDB", -10
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
        filter(type != "control") %>% 
        dplyr::select(core, type, treatment, fraction, d13C_VPDB, C_mg_g, C13_mg_g) %>% 
        pivot_longer(-c(core, type, treatment, fraction)) %>% 
        group_by(type, fraction, name) %>% 
        do(fit_hsd(.)) # %>% pivot_wider(names_from = "name", values_from = "label")
    }
    desorption_hsd_label = compute_hsd_desorption(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
    
    
  # c. then, merge the labels with the y
  desorption_hsd_label %>% left_join(desorption_label_y, by = c( "fraction", "name")) %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
}
  hsd_labels_d13c = make_hsd_labels(combined_data_processed)
  
  make_aov_labels = function(combined_data_processed){
  ## AOV_Y
  aov_y = tribble(
    ~fraction, ~name, ~y,
    "respiration", "d13C_VPDB", -300,
    "weoc", "d13C_VPDB", -40,
    "soil", "d13C_VPDB", -35
  )
  
  ## AOV
  make_control = function(combined_data_processed){
    dat_control = combined_data_processed %>% filter(type == "control") %>% mutate(type2 = "CONTROL")
    dat1 = dat_control %>% mutate(type = "sorbed-C", treatment = "1-time-zero")
    dat2 = dat_control %>% mutate(type = "sorbed-C", treatment = "2-wetting")
    dat3 = dat_control %>% mutate(type = "sorbed-C", treatment = "3-drying")
    dat4 = dat_control %>% mutate(type = "sorbed-C", treatment = "4-drying-rewetting")
    dat5 = dat_control %>% mutate(type = "solution-C", treatment = "1-time-zero")
    dat6 = dat_control %>% mutate(type = "solution-C", treatment = "2-wetting")
    
    rbind(dat1, dat2, dat3, dat4, dat5, dat6)
  }
  control = make_control(combined_data_processed)
  
  fit_aov = function(dat){
#    dat2 = dat %>% rbind(control)
    a = aov(d13C_VPDB ~ type2, data = dat)
    broom::tidy(a) %>% 
      filter(term == "type2") %>% 
      rename(p_value = `p.value`) %>% 
      mutate(label = case_when(p_value <= 0.05 ~ "*"))
  }
  aov = combined_data_processed %>% 
    filter(type != "control") %>% 
    mutate(type2 = "TRT") %>% rbind(control) %>% 
    group_by(type, treatment, fraction) %>%  #dplyr::summarise(n = n())
    do(fit_aov(.))
  
  aov %>% left_join(aov_y)
}
  aov_labels_d13c = make_aov_labels(combined_data_processed) %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))

  # third, make graphs
  
  gg_desorp_d13c = 
    combined_data_processed %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
    filter(!type %in% c("control")) %>% 
    ggplot(aes(x = treatment, y = d13C_VPDB, color = type))+
    geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
    geom_point(size = 3, show.legend = FALSE) +
    geom_text(data = hsd_labels_d13c %>% filter(type == "sorbed-C"), aes(label = label, y = y), color = "black")+
      geom_text(data = aov_labels_d13c %>% filter(type == "sorbed-C"), aes(label = label, y = y), color = "black", size = 6)+
    #scale_color_manual(values = pnw_palette("Sailboat", 3))+
    scale_color_manual(values = c(NA, "#e89c81"))+
      scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
      labs(title = "sorbed-C",
         caption = "dashed line = avg of control samples",
         x = "",
         y = "δ13C (VPDB)")+
    facet_grid(fraction~., scales = "free_y")+
    theme_kp()+
      theme(panel.grid.minor = element_blank(),
            strip.text.y = element_blank())+
    NULL
  
  gg_priming_13c = 
      combined_data_processed %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
      filter(!type %in% c("control")) %>% 
      ggplot(aes(x = treatment, y = d13C_VPDB))+
      geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
      geom_point(aes(color = type), size = 3, show.legend = FALSE) +
      geom_text(data = hsd_labels_d13c %>% filter(type == "solution-C"), aes(label = label, y = y), color = "black")+
      geom_text(data = aov_labels_d13c %>% filter(type == "solution-C"), aes(label = label, y = y), color = "black", size = 6)+
      #scale_color_manual(values = pnw_palette("Sailboat", 3))+
      scale_color_manual(values = c("#6e7cb9", NA))+
      scale_x_discrete(limits = c("1-time-zero", "2-wetting"),
                       breaks = c("1-time-zero", "2-wetting"),
                       labels = c("T0", "+C"))+
      #annotate("text", label = "soil+clay", x = 1, y = 0)+
      labs(title = "solution-C",
           subtitle = "soil + clay; solution added",
           caption = "",
           x = "",
           y = "")+
      facet_grid(fraction~., scales = "free_y")+
      theme_kp()+
      theme(panel.grid.minor = element_blank(),
            axis.text.y = element_blank())+
      NULL  
  #library(patchwork)
  gg_desorp_d13c+gg_priming_13c + plot_layout(widths = c(2,1))
}

make_graphs_c = function(combined_data_processed){
  ## UPDATE 2021-01-29: THIS IS NOW DEFUNCT
  
  # first, calculate mean values for control soils. use this in the graphs
  control_summary = 
    combined_data_processed %>% 
    filter(type %in% c("control")) %>% 
    group_by(fraction) %>% 
    dplyr::summarise(C_mg_g = mean(C_mg_g)) %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
  
  # second, prepare graph labels
  make_hsd_labels = function(combined_data_processed){
    
    # a. set y-axis values
    desorption_label_y = tribble(
      ~analysis, ~fraction, ~name, ~y,
      "HSD",  "respiration", "C_mg_g", 0.15,
      "HSD",  "weoc", "C_mg_g", 0.15,
      "HSD",  "soil", "C_mg_g", 35
    )
    
    # b. then, calculate HSD for labels 
    compute_hsd_desorption = function(combined_data_processed){
      fit_hsd = function(dat){
        a = aov(C_mg_g ~ treatment, data = dat)
        h = HSD.test(a, "treatment")
        h$groups %>% mutate(treatment = row.names(.)) %>% 
          rename(label = groups) %>%  
          dplyr::select(treatment, label)
      }
      
      combined_data_processed %>% 
        filter(type != "control") %>% 
        dplyr::select(core, type, treatment, fraction, C_mg_g) %>% 
        #pivot_longer(-c(core, type, treatment, fraction)) %>% 
        group_by(type, fraction) %>% 
        do(fit_hsd(.)) # %>% pivot_wider(names_from = "name", values_from = "label")
    }
    desorption_hsd_label = compute_hsd_desorption(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
    
    
    # c. then, merge the labels with the y
    desorption_hsd_label %>% left_join(desorption_label_y) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
  }
  hsd_labels_c = make_hsd_labels(combined_data_processed) %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
  
  
  make_aov_labels = function(combined_data_processed){
    ## AOV_Y
    aov_y = tribble(
      ~fraction, ~name, ~y,
      "respiration", "C_mg_g", -0.05,
      "weoc", "C_mg_g", 0,
      "soil", "C_mg_g", 25
    )
    
    ## AOV
    make_control = function(combined_data_processed){
      dat_control = combined_data_processed %>% filter(type == "control") %>% mutate(type2 = "CONTROL")
      dat1 = dat_control %>% mutate(type = "sorbed-C", treatment = "1-time-zero")
      dat2 = dat_control %>% mutate(type = "sorbed-C", treatment = "2-wetting")
      dat3 = dat_control %>% mutate(type = "sorbed-C", treatment = "3-drying")
      dat4 = dat_control %>% mutate(type = "sorbed-C", treatment = "4-drying-rewetting")
      dat5 = dat_control %>% mutate(type = "solution-C", treatment = "1-time-zero")
      dat6 = dat_control %>% mutate(type = "solution-C", treatment = "2-wetting")
      
      rbind(dat1, dat2, dat3, dat4, dat5, dat6)
    }
    control = make_control(combined_data_processed)
    
    fit_aov = function(dat){
      # dat2 = dat %>% rbind(control)
      a = aov(C_mg_g ~ type2, data = dat)
      broom::tidy(a) %>% 
        filter(term == "type2") %>% 
        rename(p_value = `p.value`) %>% 
        mutate(label = case_when(p_value <= 0.05 ~ "*"))
    }
    aov = combined_data_processed %>% 
      filter(type != "control") %>% 
      mutate(type2 = "TRT") %>% rbind(control) %>% 
      group_by(type, treatment, fraction) %>%  do(fit_aov(.))
    
    aov %>% left_join(aov_y)
  }
  aov_labels_c = make_aov_labels(combined_data_processed) %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
  
  gg_desorp_c = 
    combined_data_processed %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
    filter(!type %in% c("control")) %>% 
    ggplot(aes(x = treatment, y = C_mg_g, color = type))+
    geom_hline(data = control_summary, aes(yintercept = C_mg_g), linetype = "dashed", color = "grey30")+
    geom_point(size = 3, show.legend = FALSE) +
    geom_text(data = hsd_labels_c %>% filter(type == "sorbed-C"), aes(label = label, y = y), color = "black")+
    geom_text(data = aov_labels_c %>% filter(type == "sorbed-C"), aes(label = label, y = y), color = "black", size = 6)+
    #scale_color_manual(values = pnw_palette("Sailboat", 3))+
    scale_color_manual(values = c(NA, "#e89c81"))+
    scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
    labs(title = "sorbed-C",
         caption = "dashed line = avg of control samples",
         x = "",
         y = "C (mg/g)")+
    facet_grid(fraction~., scales = "free_y")+
    theme_kp()+
    theme(panel.grid.minor = element_blank(),
          strip.text.y = element_blank())+
    NULL
  
  gg_priming_c = 
    combined_data_processed %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
    filter(!type %in% c("control")) %>% 
    ggplot(aes(x = treatment, y = C_mg_g))+
    geom_hline(data = control_summary, aes(yintercept = C_mg_g), linetype = "dashed", color = "grey30")+
    geom_point(aes(color = type), size = 3, show.legend = FALSE) +
    geom_text(data = hsd_labels_c %>% filter(type == "solution-C"), aes(label = label, y = y), color = "black")+
    geom_text(data = aov_labels_c %>% filter(type == "solution-C"), aes(label = label, y = y), color = "black", size = 6)+
    #scale_color_manual(values = pnw_palette("Sailboat", 3))+
    scale_color_manual(values = c("#6e7cb9", NA))+
    scale_x_discrete(limits = c("1-time-zero", "2-wetting"),
                     breaks = c("1-time-zero", "2-wetting"),
                     labels = c("T0", "+C"))+
    #annotate("text", label = "soil+clay", x = 1, y = 0)+
    labs(title = "solution-C",
         subtitle = "soil + clay; solution added",
         caption = "",
         x = "",
         y = "")+
    facet_grid(fraction~., scales = "free_y")+
    theme_kp()+
    theme(panel.grid.minor = element_blank(),
          axis.text.y = element_blank()
    )+
    NULL 
  #library(patchwork)
  gg_desorp_c+gg_priming_c + plot_layout(widths = c(2,1))
  
}

make_graphs_desorption = function(combined_data_processed){
  make_graphs_desorption_d13c = function(combined_data_processed){
    # first, calculate mean values for control soils. use this in the graphs ----
    control_summary = 
      combined_data_processed %>% 
      filter(type %in% c("control")) %>% 
      group_by(fraction) %>% 
      dplyr::summarise(d13C_VPDB = mean(d13C_VPDB)) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # second, prepare graph labels ----
    make_hsd_labels = function(combined_data_processed){
      # a. set y-axis values
      desorption_label_y = tribble(
        ~analysis, ~fraction, ~name, ~y,
        "HSD",  "respiration", "d13C_VPDB", 2100,
        "HSD",  "weoc", "d13C_VPDB", 30,
        "HSD",  "weoc_pellet", "d13C_VPDB", -10,
        "HSD",  "soil", "d13C_VPDB", -10
      )
      
      # b. then, calculate HSD for labels 
      compute_hsd_desorption = function(combined_data_processed){
        fit_hsd = function(dat){
          a = aov(d13C_VPDB ~ treatment, data = dat)
          h = HSD.test(a, "treatment")
          h$groups %>% mutate(treatment = row.names(.)) %>% 
            rename(label = groups) %>%  
            dplyr::select(treatment, label)
        }
        
        combined_data_processed %>% 
          filter(type == "sorbed-C") %>% 
          dplyr::select(core, type, treatment, fraction, d13C_VPDB) %>% 
          #          pivot_longer(-c(core, type, treatment, fraction)) %>% 
          group_by(type, fraction) %>% 
          do(fit_hsd(.)) # %>% pivot_wider(names_from = "name", values_from = "label")
      }
      desorption_hsd_label = compute_hsd_desorption(combined_data_processed) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
      
      
      # c. then, merge the labels with the y
      desorption_hsd_label %>% left_join(desorption_label_y) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    }
    hsd_labels_d13c = make_hsd_labels(combined_data_processed)
    
    make_aov_labels = function(combined_data_processed){
      ## AOV_Y
      aov_y = tribble(
        ~fraction, ~name, ~y,
        "respiration", "d13C_VPDB", -300,
        "weoc", "d13C_VPDB", -40,
        "weoc_pellet", "d13C_VPDB", -35,
        "soil", "d13C_VPDB", -35
      )
      
      ## AOV
      make_control = function(combined_data_processed){
        dat_control = combined_data_processed %>% filter(type == "control") %>% mutate(type2 = "CONTROL")
        dat1 = dat_control %>% mutate(type = "sorbed-C", treatment = "1-time-zero")
        dat2 = dat_control %>% mutate(type = "sorbed-C", treatment = "2-wetting")
        dat3 = dat_control %>% mutate(type = "sorbed-C", treatment = "3-drying")
        dat4 = dat_control %>% mutate(type = "sorbed-C", treatment = "4-drying-rewetting")
        
        rbind(dat1, dat2, dat3, dat4)
      }
      control = make_control(combined_data_processed)
      
      fit_aov = function(dat){
        #    dat2 = dat %>% rbind(control)
        a = aov(d13C_VPDB ~ type2, data = dat)
        broom::tidy(a) %>% 
          filter(term == "type2") %>% 
          rename(p_value = `p.value`) %>% 
          mutate(label = case_when(p_value <= 0.05 ~ "*"))
      }
      aov = combined_data_processed %>% 
        filter(type == "sorbed-C") %>% 
        mutate(type2 = "TRT") %>% rbind(control) %>% 
        group_by(type, treatment, fraction) %>%  #dplyr::summarise(n = n())
        do(fit_aov(.))
      
      aov %>% left_join(aov_y)
    }
    aov_labels_d13c = make_aov_labels(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # third, make graphs ----
    combined_data_processed %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil"))) %>% 
      filter(!type %in% c("control")) %>% 
      ggplot(aes(x = treatment, y = d13C_VPDB, color = type))+
      geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
      geom_point(size = 3, show.legend = FALSE) +
      geom_text(data = hsd_labels_d13c %>% filter(type == "sorbed-C"), aes(label = label, y = y), color = "black")+
      geom_text(data = aov_labels_d13c %>% filter(type == "sorbed-C"), aes(label = label, y = y), color = "black", size = 6)+
      #scale_color_manual(values = pnw_palette("Sailboat", 3))+
      scale_color_manual(values = c(NA, "#e89c81"))+
      scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
      labs(title = "δ13C (‰)",
           #caption = "dashed line = avg of control samples",
           x = "",
           y = "")+
      facet_grid(fraction~., scales = "free_y")+
      theme_kp()+
      theme(panel.grid.minor = element_blank(),
            strip.text.y = element_blank())+
      NULL
  }
  gg_desorp_d13c = make_graphs_desorption_d13c(combined_data_processed)
  
  make_graphs_desorption_c = function(combined_data_processed){
    # first, calculate mean values for control soils. use this in the graphs ----
    control_summary = 
      combined_data_processed %>% 
      filter(type %in% c("control")) %>% 
      group_by(fraction) %>% 
      dplyr::summarise(C_mg_g = mean(C_mg_g)) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # second, prepare graph labels ----
    make_hsd_labels = function(combined_data_processed){
      # a. set y-axis values
      desorption_label_y = tribble(
        ~analysis, ~fraction, ~name, ~y,
        "HSD",  "respiration", "C_mg_g", 0.15,
        "HSD",  "weoc", "C_mg_g", 0.15,
        "HSD",  "weoc_pellet", "C_mg_g", 32,
        "HSD",  "soil", "C_mg_g", 32
      )
      
      # b. then, calculate HSD for labels 
      compute_hsd_desorption = function(combined_data_processed){
        fit_hsd = function(dat){
          a = aov(C_mg_g ~ treatment, data = dat)
          h = HSD.test(a, "treatment")
          h$groups %>% mutate(treatment = row.names(.)) %>% 
            rename(label = groups) %>%  
            dplyr::select(treatment, label)
        }
        
        combined_data_processed %>% 
          filter(type == "sorbed-C") %>% 
          dplyr::select(core, type, treatment, fraction, C_mg_g) %>% 
          #pivot_longer(-c(core, type, treatment, fraction)) %>% 
          group_by(type, fraction) %>% 
          do(fit_hsd(.)) # %>% pivot_wider(names_from = "name", values_from = "label")
      }
      desorption_hsd_label = compute_hsd_desorption(combined_data_processed) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
      
      # c. then, merge the labels with the y
      desorption_hsd_label %>% left_join(desorption_label_y) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    }
    hsd_labels_c = make_hsd_labels(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    make_aov_labels = function(combined_data_processed){
      ## AOV_Y
      aov_y = tribble(
        ~fraction, ~name, ~y,
        "respiration", "C_mg_g", -0.02,
        "weoc", "C_mg_g", 0,
        "weoc_pellet", "C_mg_g", 22,
        "soil", "C_mg_g", 25
      )
      
      ## AOV
      make_control = function(combined_data_processed){
        dat_control = combined_data_processed %>% filter(type == "control") %>% mutate(type2 = "CONTROL")
        dat1 = dat_control %>% mutate(type = "sorbed-C", treatment = "1-time-zero")
        dat2 = dat_control %>% mutate(type = "sorbed-C", treatment = "2-wetting")
        dat3 = dat_control %>% mutate(type = "sorbed-C", treatment = "3-drying")
        dat4 = dat_control %>% mutate(type = "sorbed-C", treatment = "4-drying-rewetting")
        
        rbind(dat1, dat2, dat3, dat4)
      }
      control = make_control(combined_data_processed)
      
      fit_aov = function(dat){
        # dat2 = dat %>% rbind(control)
        a = aov(C_mg_g ~ type2, data = dat)
        broom::tidy(a) %>% 
          filter(term == "type2") %>% 
          rename(p_value = `p.value`) %>% 
          mutate(label = case_when(p_value <= 0.05 ~ "*"))
      }
      aov = combined_data_processed %>% 
        filter(type == "sorbed-C") %>% 
        mutate(type2 = "TRT") %>% rbind(control) %>% 
        group_by(type, treatment, fraction) %>%  do(fit_aov(.))
      
      aov %>% left_join(aov_y)
    }
    aov_labels_c = make_aov_labels(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # third, make graph ----
    combined_data_processed %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil"))) %>% 
      filter(!type %in% c("control")) %>% 
      ggplot(aes(x = treatment, y = C_mg_g, color = type))+
      geom_hline(data = control_summary, aes(yintercept = C_mg_g), linetype = "dashed", color = "grey30")+
      geom_point(size = 3, show.legend = FALSE) +
      geom_text(data = hsd_labels_c %>% filter(type == "sorbed-C"), aes(label = label, y = y), color = "black")+
      geom_text(data = aov_labels_c %>% filter(type == "sorbed-C"), aes(label = label, y = y), color = "black", size = 6)+
      #scale_color_manual(values = pnw_palette("Sailboat", 3))+
      scale_color_manual(values = c(NA, "#e89c81"))+
      scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
      labs(title = "C (mg/g)",
           #caption = "dashed line = avg of control samples",
           x = "",
           y = "")+
      facet_grid(fraction~., scales = "free_y")+
      theme_kp()+
      theme(panel.grid.minor = element_blank(),
            #strip.text.y = element_blank()
            )+
      NULL
  }
  gg_desorp_c = make_graphs_desorption_c(combined_data_processed)
  
  make_graphs_desorption_c13 = function(combined_data_processed){
    # first, calculate mean values for control soils. use this in the graphs ----
    control_summary = 
      combined_data_processed %>% 
      filter(type %in% c("control")) %>% 
      group_by(fraction) %>% 
      dplyr::summarise(C13_mg_g = mean(C13_mg_g)) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # second, prepare graph labels ----
    make_hsd_labels = function(combined_data_processed){
      # a. set y-axis values
      desorption_label_y = tribble(
        ~analysis, ~fraction, ~name, ~y,
        "HSD",  "respiration", "C13_mg_g", 3.5,
        "HSD",  "weoc", "C13_mg_g", 1.5,
        "HSD",  "weoc_pellet", "C13_mg_g", 400,
        "HSD",  "soil", "C13_mg_g", 400
      )
      
      # b. then, calculate HSD for labels 
      compute_hsd_desorption = function(combined_data_processed){
        fit_hsd = function(dat){
          a = aov(C13_mg_g ~ treatment, data = dat)
          h = HSD.test(a, "treatment")
          h$groups %>% mutate(treatment = row.names(.)) %>% 
            rename(label = groups) %>%  
            dplyr::select(treatment, label)
        }
        
        combined_data_processed %>% 
          filter(type == "sorbed-C") %>% 
          dplyr::select(core, type, treatment, fraction, C13_mg_g) %>% 
          #pivot_longer(-c(core, type, treatment, fraction)) %>% 
          group_by(type, fraction) %>% 
          do(fit_hsd(.)) # %>% pivot_wider(names_from = "name", values_from = "label")
      }
      desorption_hsd_label = compute_hsd_desorption(combined_data_processed) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
      
      # c. then, merge the labels with the y
      desorption_hsd_label %>% left_join(desorption_label_y) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    }
    hsd_labels_c = make_hsd_labels(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    make_aov_labels = function(combined_data_processed){
      ## AOV_Y
      aov_y = tribble(
        ~fraction, ~name, ~y,
        "respiration", "C13_mg_g", -0.5,
        "weoc", "C13_mg_g", 0,
        "weoc_pellet", "C13_mg_g", 250,
        "soil", "C13_mg_g", 250
      )
      
      ## AOV
      make_control = function(combined_data_processed){
        dat_control = combined_data_processed %>% filter(type == "control") %>% mutate(type2 = "CONTROL")
        dat1 = dat_control %>% mutate(type = "sorbed-C", treatment = "1-time-zero")
        dat2 = dat_control %>% mutate(type = "sorbed-C", treatment = "2-wetting")
        dat3 = dat_control %>% mutate(type = "sorbed-C", treatment = "3-drying")
        dat4 = dat_control %>% mutate(type = "sorbed-C", treatment = "4-drying-rewetting")
        
        rbind(dat1, dat2, dat3, dat4)
      }
      control = make_control(combined_data_processed)
      
      fit_aov = function(dat){
        # dat2 = dat %>% rbind(control)
        a = aov(C13_mg_g ~ type2, data = dat)
        broom::tidy(a) %>% 
          filter(term == "type2") %>% 
          rename(p_value = `p.value`) %>% 
          mutate(label = case_when(p_value <= 0.05 ~ "*"))
      }
      aov = combined_data_processed %>% 
        filter(type == "sorbed-C") %>% 
        mutate(type2 = "TRT") %>% rbind(control) %>% 
        group_by(type, treatment, fraction) %>%  do(fit_aov(.))
      
      aov %>% left_join(aov_y)
    }
    aov_labels_c = make_aov_labels(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # third, make graph ----
    combined_data_processed %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil"))) %>% 
      filter(!type %in% c("control")) %>% 
      ggplot(aes(x = treatment, y = C13_mg_g*1000, color = type))+
      geom_hline(data = control_summary, aes(yintercept = C13_mg_g*1000), linetype = "dashed", color = "grey30")+
      geom_point(size = 3, show.legend = FALSE) +
      geom_text(data = hsd_labels_c %>% filter(type == "sorbed-C"), aes(label = label, y = y), color = "black")+
      geom_text(data = aov_labels_c %>% filter(type == "sorbed-C"), aes(label = label, y = y), color = "black", size = 6)+
      #scale_color_manual(values = pnw_palette("Sailboat", 3))+
      scale_color_manual(values = c(NA, "#e89c81"))+
      scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
      labs(title = "C13 (μg/g)",
           #caption = "dashed line = avg of control samples",
           x = "",
           y = "")+
      facet_grid(fraction~., scales = "free_y")+
      theme_kp()+
      theme(panel.grid.minor = element_blank(),
            #strip.text.y = element_blank()
      )+
      NULL
  }
  gg_desorp_c13 = make_graphs_desorption_c13(combined_data_processed)
  
  gg_desorp_c + gg_desorp_d13c + gg_desorp_c13 + plot_annotation(caption = "dashed line = avg of control samples") 
}

make_graphs_priming = function(combined_data_processed){
  make_graphs_priming_d13c = function(combined_data_processed){
    # first, calculate mean values for control soils. use this in the graphs ----
    control_summary = 
      combined_data_processed %>% 
      filter(type %in% c("control")) %>% 
      group_by(fraction) %>% 
      dplyr::summarise(d13C_VPDB = mean(d13C_VPDB)) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # second, prepare graph labels ----
    make_hsd_labels = function(combined_data_processed){
      # a. set y-axis values
      desorption_label_y = tribble(
        ~analysis, ~fraction, ~name, ~y,
        "HSD",  "respiration", "d13C_VPDB", 2100,
        "HSD",  "weoc", "d13C_VPDB", 30,
        "HSD",  "weoc_pellet", "d13C_VPDB", -10,
        "HSD",  "soil", "d13C_VPDB", -10
      )
      
      # b. then, calculate HSD for labels 
      compute_hsd_desorption = function(combined_data_processed){
        fit_hsd = function(dat){
          a = aov(d13C_VPDB ~ treatment, data = dat)
          h = HSD.test(a, "treatment")
          h$groups %>% mutate(treatment = row.names(.)) %>% 
            rename(label = groups) %>%  
            dplyr::select(treatment, label)
        }
        
        combined_data_processed %>% 
          filter(type == "solution-C") %>% 
          dplyr::select(core, type, treatment, fraction, d13C_VPDB) %>% 
          #          pivot_longer(-c(core, type, treatment, fraction)) %>% 
          group_by(type, fraction) %>% 
          do(fit_hsd(.)) # %>% pivot_wider(names_from = "name", values_from = "label")
      }
      desorption_hsd_label = compute_hsd_desorption(combined_data_processed) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
      
      
      # c. then, merge the labels with the y
      desorption_hsd_label %>% left_join(desorption_label_y) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    }
    hsd_labels_d13c = make_hsd_labels(combined_data_processed)
    
    make_aov_labels = function(combined_data_processed){
      ## AOV_Y
      aov_y = tribble(
        ~fraction, ~name, ~y,
        "respiration", "d13C_VPDB", -300,
        "weoc", "d13C_VPDB", -40,
        "weoc_pellet", "d13C_VPDB", -35,
        "soil", "d13C_VPDB", -35
      )
      
      ## AOV
      make_control = function(combined_data_processed){
        dat_control = combined_data_processed %>% filter(type == "control") %>% mutate(type2 = "CONTROL")
        dat1 = dat_control %>% mutate(type = "solution-C", treatment = "1-time-zero")
        dat2 = dat_control %>% mutate(type = "solution-C", treatment = "2-wetting")
        
        rbind(dat1, dat2)
      }
      control = make_control(combined_data_processed)
      
      fit_aov = function(dat){
        #    dat2 = dat %>% rbind(control)
        a = aov(d13C_VPDB ~ type2, data = dat)
        broom::tidy(a) %>% 
          filter(term == "type2") %>% 
          rename(p_value = `p.value`) %>% 
          mutate(label = case_when(p_value <= 0.05 ~ "*"))
      }
      aov = combined_data_processed %>% 
        filter(type == "solution-C") %>% 
        mutate(type2 = "TRT") %>% rbind(control) %>% 
        group_by(type, treatment, fraction) %>%  #dplyr::summarise(n = n())
        do(fit_aov(.))
      
      aov %>% left_join(aov_y)
    }
    aov_labels_d13c = make_aov_labels(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # third, make graphs ----
    combined_data_processed %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil"))) %>% 
      # filter(type %in% c("solution-C")) %>% 
      filter(!type %in% c("control")) %>% 
      ggplot(aes(x = treatment, y = d13C_VPDB, color = type))+
      geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
      geom_point(size = 3, show.legend = FALSE) +
      geom_text(data = hsd_labels_d13c %>% filter(type == "solution-C"), aes(label = label, y = y), color = "black")+
      geom_text(data = aov_labels_d13c %>% filter(type == "solution-C"), aes(label = label, y = y), color = "black", size = 6)+
      #scale_color_manual(values = pnw_palette("Sailboat", 3))+
      scale_color_manual(values = c("#6e7cb9", NA))+
      #scale_x_discrete(labels = c("T0", "+C"))+
      scale_x_discrete(limits = c("1-time-zero", "2-wetting"),
                       breaks = c("1-time-zero", "2-wetting"),
                       labels = c("T0", "+C"))+
      labs(title = "δ13C (‰)",
           #caption = "dashed line = avg of control samples",
           x = "",
           y = "")+
      facet_grid(fraction~., scales = "free_y")+
      theme_kp()+
      theme(panel.grid.minor = element_blank(),
            strip.text.y = element_blank())+
      NULL
  }
  gg_priming_d13c = make_graphs_priming_d13c(combined_data_processed)
  
  make_graphs_priming_c = function(combined_data_processed){
    # first, calculate mean values for control soils. use this in the graphs ----
    control_summary = 
      combined_data_processed %>% 
      filter(type %in% c("control")) %>% 
      group_by(fraction) %>% 
      dplyr::summarise(C_mg_g = mean(C_mg_g)) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # second, prepare graph labels ----
    make_hsd_labels = function(combined_data_processed){
      # a. set y-axis values
      desorption_label_y = tribble(
        ~analysis, ~fraction, ~name, ~y,
        "HSD",  "respiration", "C_mg_g", 0.15,
        "HSD",  "weoc", "C_mg_g", 0.15,
        "HSD",  "weoc_pellet", "C_mg_g", 32,
        "HSD",  "soil", "C_mg_g", 32
      )
      
      # b. then, calculate HSD for labels 
      compute_hsd_desorption = function(combined_data_processed){
        fit_hsd = function(dat){
          a = aov(C_mg_g ~ treatment, data = dat)
          h = HSD.test(a, "treatment")
          h$groups %>% mutate(treatment = row.names(.)) %>% 
            rename(label = groups) %>%  
            dplyr::select(treatment, label)
        }
        
        combined_data_processed %>% 
          filter(type == "solution-C") %>% 
          dplyr::select(core, type, treatment, fraction, C_mg_g) %>% 
          #pivot_longer(-c(core, type, treatment, fraction)) %>% 
          group_by(type, fraction) %>% 
          do(fit_hsd(.)) # %>% pivot_wider(names_from = "name", values_from = "label")
      }
      desorption_hsd_label = compute_hsd_desorption(combined_data_processed) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
      
      # c. then, merge the labels with the y
      desorption_hsd_label %>% left_join(desorption_label_y) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    }
    hsd_labels_c = make_hsd_labels(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    make_aov_labels = function(combined_data_processed){
      ## AOV_Y
      aov_y = tribble(
        ~fraction, ~name, ~y,
        "respiration", "C_mg_g", -0.02,
        "weoc", "C_mg_g", 0,
        "weoc_pellet", "C_mg_g", 22,
        "soil", "C_mg_g", 25
      )
      
      ## AOV
      make_control = function(combined_data_processed){
        dat_control = combined_data_processed %>% filter(type == "control") %>% mutate(type2 = "CONTROL")
        dat1 = dat_control %>% mutate(type = "solution-C", treatment = "1-time-zero")
        dat2 = dat_control %>% mutate(type = "solution-C", treatment = "2-wetting")
        
        rbind(dat1, dat2)
      }
      control = make_control(combined_data_processed)
      
      fit_aov = function(dat){
        # dat2 = dat %>% rbind(control)
        a = aov(C_mg_g ~ type2, data = dat)
        broom::tidy(a) %>% 
          filter(term == "type2") %>% 
          rename(p_value = `p.value`) %>% 
          mutate(label = case_when(p_value <= 0.05 ~ "*"))
      }
      aov = combined_data_processed %>% 
        filter(type == "solution-C") %>% 
        mutate(type2 = "TRT") %>% rbind(control) %>% 
        group_by(type, treatment, fraction) %>%  do(fit_aov(.))
      
      aov %>% left_join(aov_y)
    }
    aov_labels_c = make_aov_labels(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # 2b, add empty labels for y axis spacing ----
    empty_label_y = tribble(
      ~analysis, ~fraction, ~name, ~y, ~label,
      "empty",  "respiration", "C_mg_g", 0.15, " ",
      "empty",  "respiration", "C_mg_g", -0.02, " ",
      "empty",  "weoc", "C_mg_g", 0.15, " ",
      "empty",  "weoc", "C_mg_g", 0, " ",
      "empty",  "weoc_pellet", "C_mg_g", 32, " ",
      "empty",  "weoc_pellet", "C_mg_g", 22, " ",
      "empty",  "soil", "C_mg_g", 32, " ",
      "empty",  "soil", "C_mg_g", 25, " "
    ) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    
    # third, make graph ----
    combined_data_processed %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil"))) %>% 
      #filter(type %in% c("solution-C")) %>% 
      filter(!type %in% "control") %>% 
      ggplot(aes(x = treatment, y = C_mg_g, color = type))+
      geom_hline(data = control_summary, aes(yintercept = C_mg_g), linetype = "dashed", color = "grey30")+
      geom_point(size = 3, show.legend = FALSE) +
      geom_text(data = hsd_labels_c %>% filter(type == "solution-C"), aes(label = label, y = y), color = "black")+
      geom_text(data = aov_labels_c %>% filter(type == "solution-C"), aes(label = label, y = y), color = "black", size = 6)+
      geom_text(data = empty_label_y, aes(label = label, y = y, x = 1.5), color = NA)+
      #scale_color_manual(values = pnw_palette("Sailboat", 3))+
      scale_color_manual(values = c("#6e7cb9", NA))+
      #scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
      scale_x_discrete(limits = c("1-time-zero", "2-wetting"),
                       breaks = c("1-time-zero", "2-wetting"),
                       labels = c("T0", "+C"))+
      labs(title = "C (mg/g)",
           #caption = "dashed line = avg of control samples",
           x = "",
           y = "")+
      facet_grid(fraction~., scales = "free_y")+
      theme_kp()+
      theme(panel.grid.minor = element_blank(),
            #strip.text.y = element_blank()
            )+
      NULL
  }
  gg_priming_c = make_graphs_priming_c(combined_data_processed)
  
  make_graphs_priming_c13 = function(combined_data_processed){
    # first, calculate mean values for control soils. use this in the graphs ----
    control_summary = 
      combined_data_processed %>% 
      filter(type %in% c("control")) %>% 
      group_by(fraction) %>% 
      dplyr::summarise(C13_mg_g = mean(C13_mg_g)) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # second, prepare graph labels ----
    make_hsd_labels = function(combined_data_processed){
      # a. set y-axis values
      desorption_label_y = tribble(
        ~analysis, ~fraction, ~name, ~y,
        "HSD",  "respiration", "C13_mg_g", 3.5,
        "HSD",  "weoc", "C13_mg_g", 1.5,
        "HSD",  "weoc_pellet", "C13_mg_g", 400,
        "HSD",  "soil", "C13_mg_g", 400
      )
      
      # b. then, calculate HSD for labels 
      compute_hsd_desorption = function(combined_data_processed){
        fit_hsd = function(dat){
          a = aov(C13_mg_g ~ treatment, data = dat)
          h = HSD.test(a, "treatment")
          h$groups %>% mutate(treatment = row.names(.)) %>% 
            rename(label = groups) %>%  
            dplyr::select(treatment, label)
        }
        
        combined_data_processed %>% 
          filter(type == "solution-C") %>% 
          dplyr::select(core, type, treatment, fraction, C13_mg_g) %>% 
          #pivot_longer(-c(core, type, treatment, fraction)) %>% 
          group_by(type, fraction) %>% 
          do(fit_hsd(.)) # %>% pivot_wider(names_from = "name", values_from = "label")
      }
      desorption_hsd_label = compute_hsd_desorption(combined_data_processed) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
      
      # c. then, merge the labels with the y
      desorption_hsd_label %>% left_join(desorption_label_y) %>% 
        mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    }
    hsd_labels_c = make_hsd_labels(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    make_aov_labels = function(combined_data_processed){
      ## AOV_Y
      aov_y = tribble(
        ~fraction, ~name, ~y,
        "respiration", "C13_mg_g", -0.5,
        "weoc", "C13_mg_g", 0,
        "weoc_pellet", "C13_mg_g", 250,
        "soil", "C13_mg_g", 250
      )
      
      ## AOV
      make_control = function(combined_data_processed){
        dat_control = combined_data_processed %>% filter(type == "control") %>% mutate(type2 = "CONTROL")
        dat1 = dat_control %>% mutate(type = "solution-C", treatment = "1-time-zero")
        dat2 = dat_control %>% mutate(type = "solution-C", treatment = "2-wetting")
        
        rbind(dat1, dat2)
      }
      control = make_control(combined_data_processed)
      
      fit_aov = function(dat){
        # dat2 = dat %>% rbind(control)
        a = aov(C13_mg_g ~ type2, data = dat)
        broom::tidy(a) %>% 
          filter(term == "type2") %>% 
          rename(p_value = `p.value`) %>% 
          mutate(label = case_when(p_value <= 0.05 ~ "*"))
      }
      aov = combined_data_processed %>% 
        filter(type == "solution-C") %>% 
        mutate(type2 = "TRT") %>% rbind(control) %>% 
        group_by(type, treatment, fraction) %>%  do(fit_aov(.))
      
      aov %>% left_join(aov_y)
    }
    aov_labels_c = make_aov_labels(combined_data_processed) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    # 2b, add empty labels for y axis spacing ----
    empty_label_y = tribble(
      ~analysis, ~fraction, ~name, ~y, ~label,
      "empty",  "respiration", "C13_mg_g", 3, " ",
      "empty",  "respiration", "C13_mg_g", 0, " ",
      "empty",  "weoc", "C13_mg_g", 1.5, " ",
      "empty",  "weoc", "C13_mg_g", 0, " ",
      "empty",  "weoc_pellet", "C13_mg_g", 400, " ",
      "empty",  "weoc_pellet", "C13_mg_g", 250, " ",
      "empty",  "soil", "C13_mg_g", 400, " ",
      "empty",  "soil", "C13_mg_g", 250, " "
    ) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")))
    
    
    # third, make graph ----
    combined_data_processed %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil"))) %>% 
      #filter(type %in% c("solution-C")) %>% 
      filter(!type %in% "control") %>% 
      ggplot(aes(x = treatment, y = C13_mg_g*1000, color = type))+
      geom_hline(data = control_summary, aes(yintercept = C13_mg_g*1000), linetype = "dashed", color = "grey30")+
      geom_point(size = 3, show.legend = FALSE) +
      geom_text(data = hsd_labels_c %>% filter(type == "solution-C"), aes(label = label, y = y), color = "black")+
      geom_text(data = aov_labels_c %>% filter(type == "solution-C"), aes(label = label, y = y), color = "black", size = 6)+
      geom_text(data = empty_label_y, aes(label = label, y = y, x = 1.5), color = NA)+
      #scale_color_manual(values = pnw_palette("Sailboat", 3))+
      scale_color_manual(values = c("#6e7cb9", NA))+
      #scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
      scale_x_discrete(limits = c("1-time-zero", "2-wetting"),
                       breaks = c("1-time-zero", "2-wetting"),
                       labels = c("T0", "+C"))+
      labs(title = "C13 (μg/g)",
           #caption = "dashed line = avg of control samples",
           x = "",
           y = "")+
      facet_grid(fraction~., scales = "free_y")+
      theme_kp()+
      theme(panel.grid.minor = element_blank(),
            #strip.text.y = element_blank()
      )+
      NULL
  }
  gg_priming_c13 = make_graphs_priming_c13(combined_data_processed)
  
  gg_priming_c + gg_priming_d13c + gg_priming_c13 + plot_annotation(caption = "dashed line = avg of control samples") 
}

calculate_mass_balance = function(combined_data_processed){
  ## 13C stacked plots
  combined_data_processed_summary = 
    combined_data_processed %>% 
    group_by(treatment, type, fraction) %>%
    dplyr::summarise(C_mg_g = mean(C_mg_g),
                     C13_mg_g = mean(C13_mg_g))
  
  ## create label for soil 13C
  ## not plotting the data because very large values
  label = combined_data_processed_summary %>% 
    filter(fraction == "soil") %>% 
    group_by(treatment, type, fraction) %>% 
    dplyr::summarise(C_mg_g = mean(C_mg_g),
                     C13_mg_g = mean(C13_mg_g),
                     C13_mg_g = round(C13_mg_g,3)) %>% 
    dplyr::select(-fraction)
  
  gg_massbalance_desorp = 
    combined_data_processed_summary %>% 
    filter(fraction != "soil" & type != "solution-C") %>% 
    ggplot(aes(x = treatment, y = C13_mg_g*1000))+
    geom_bar(aes(fill = fraction, color = fraction), stat = "identity", position = position_dodge(width = 0.7), width = 0.5, alpha = 0.7, size = 0.7)+
    geom_text(data = label %>%  filter(type != "solution-C"), aes(y = 2.95, label = C13_mg_g*1000))+
    annotate("text", label = "total 13C in soil (μg/g):", x = 0.7, y = 3.10, hjust = 0)+
    labs(x = "", y = "13C (μg/g)")+
    scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
    #scale_fill_manual(values = pnw_palette("Sunset", 3))+
    scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
    scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
    facet_wrap(~type)+
    theme_kp()+
    #theme(axis.text.x = element_text(angle = 45))+
    NULL

  gg_massbalance_priming = 
    combined_data_processed_summary %>% 
    filter(fraction != "soil" & type == "solution-C") %>% 
    ggplot(aes(x = treatment, y = C13_mg_g*1000))+
    geom_bar(aes(fill = fraction, color = fraction), stat = "identity", position = position_dodge(width = 0.7), width = 0.5, alpha = 0.7, size = 0.7)+
    geom_text(data = label %>%  filter(type == "solution-C"), aes(y = 2.95, label = C13_mg_g*1000))+
    annotate("text", label = "total 13C in soil (μg/g):", x = 0.7, y = 3.10, hjust = 0)+
    labs(x = "", y = "13C (μg/g)")+
    scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
    scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
    scale_x_discrete(labels = c("T0", "+C"))+
    facet_wrap(~type)+
    theme_kp()+
    #theme(axis.text.x = element_text(angle = 45))+
    NULL
  
  
  gg_mass_balance_soilc = 
    combined_data_processed_summary %>% 
    filter(fraction != "soil") %>% 
    ggplot(aes(x = treatment, y = C_mg_g))+
    geom_bar(aes(fill = fraction), stat = "identity")+
    geom_text(data = label, aes(y = 0.25, label = C_mg_g))+
      annotate("text", label = "total C in soil (mg/g):", x = 0.7, y = 0.28, hjust = 0)+
      facet_wrap(~type)+
    theme_kp()+
    theme(axis.text.x = element_text(angle = 45))+
    NULL
  
  list(gg_massbalance_desorp = gg_massbalance_desorp, 
       gg_massbalance_priming = gg_massbalance_priming,
       gg_mass_balance_soilc = gg_mass_balance_soilc)
}

calculate_clay_effect = function(combined_data_processed){
  clay = combined_data_processed %>% filter(treatment == "1-time-zero" & type != "sorbed-C") %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
  
  make_tzero_label = function(clay){
    label_y = tribble(
      ~fraction, ~d13C_y, ~C_y,
      "respiration", -55, 0,
      "soil", -28, 27,
      "weoc", -28, 0)
    
    fit_aov_c = function(dat){
      
      a = aov(value ~ type, data = dat)
      broom::tidy(a) %>% 
        filter(term == "type") %>% 
        rename(p_value = `p.value`) %>% 
        mutate(label = case_when(p_value <= 0.05 ~ "*"))
    }
    
    clay %>% 
      dplyr::select(type, treatment, fraction, C_mg_g, d13C_VPDB) %>% 
      pivot_longer(-c(type, treatment, fraction)) %>% 
      group_by(fraction, name) %>% 
      do(fit_aov_c(.)) %>% 
      dplyr::select(fraction, name, label) %>% 
      pivot_wider(names_from = "name", values_from = "label") %>% 
      left_join(label_y) %>% 
      mutate(type = "a") %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil")))
  }
  tzero_label = make_tzero_label(clay)
  
  gg_tzero_d13c = 
    clay %>% 
    ggplot(aes(x = type, y = d13C_VPDB, color = type))+
    geom_point(aes(shape = type), size = 3, stroke = 1, show.legend = F, color = "black")+
    geom_text(data = tzero_label, aes(x = 1.5, y = d13C_y, label = d13C_VPDB), color = "black", size = 7)+  
    labs(title = "δ13C (‰)",
         x = "", y = "")+
    scale_x_discrete(labels = c("control", "+ clay"))+
    #scale_color_manual(values = c("purple", "yellow"))+
    scale_shape_manual(values = c(1, 19))+
    facet_grid(fraction~., scales = "free_y")+
    theme_kp()+
    NULL
  
  gg_tzero_c = 
    clay %>% 
    ggplot(aes(x = type, y = C_mg_g, color = type))+
      geom_point(aes(shape = type), size = 3, stroke = 1, show.legend = F, color = "black")+
      geom_text(data = tzero_label, aes(x = 1.5, y = C_y, label = C_mg_g), color = "black", size = 7)+  
      labs(title = "C (mg/g)",
           x = "", y = "")+
      scale_x_discrete(labels = c("control", "+ clay"))+
      #scale_color_manual(values = c("purple", "yellow"))+
      scale_shape_manual(values = c(1, 19))+
      facet_grid(fraction~., scales = "free_y")+
    theme_kp()+
      theme(strip.text.y = element_blank())+
    NULL
  
  
  gg_tzero_c + gg_tzero_d13c 

}

calculate_recovery = function(){
  
  compute_priming_recovery = function(){
    combined_data_processed %>% 
      filter(type == "solution-C" & fraction %in% c("respiration", "soil")) %>% 
      group_by(core, treatment) %>% 
      dplyr::summarise(C13_mg_g = sum(C13_mg_g)) %>% 
      group_by(treatment) %>% 
      dplyr::summarise(C13_ug_g = mean(C13_mg_g*1000))
    
    
    combined_data_processed %>% 
      filter(type == "solution-C" & fraction %in% c("respiration", "soil")) %>% 
      group_by(treatment, fraction) %>% 
      dplyr::summarise(C13_ug_g = mean(C13_mg_g*1000))
  }
  
  
  
  
  
}

# mixing model for respiration --------------------------------------------

compute_mixing_model = function(){
OXALIC_D13C = 15098.6 # permille

resp_fraction = 
  combined_data_key %>% 
  filter(fraction == "respiration") %>% 
  mutate(f_oxalic = (d13C_VPDB - CONTROL_WEOC_D13C)/(OXALIC_D13C - CONTROL_WEOC_D13C))

resp_fraction %>% 
  filter(type == "solution-C") %>% 
  ggplot(aes(x = treatment, y = f_oxalic * 100, color = type))+
  geom_point()+
  labs(title = "% contribution of oxalic-13C to respiration")

summary(aov(D13C_calc ~ treatment,
            data = combined_lgr_licor %>% 
              filter(type == "sorbed-C" & treatment %in% c("2-wetting", "4-drying-rewetting"))
))

}


# -------------------------------------------------------------------------


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
##                    filter(type %in% c("control", "solution-C") &
##                             treatment == "1-time-zero" &
##                             core != 40))
##            )
##            
##            a_priming = 
##              a %>% 
##              filter(type %in% "solution-C" & treatment == "1-time-zero")
##            
##            dixon.test(a_priming %>% pull(CO2_ppm),
##                       type = 0, opposite = FALSE, two.sided = TRUE) %>% 
##              summary()
##            
##            
##          





###############
###############
# combine sorption and solution C -----------------------------------------

plot_sorbed_and_solution = function(combined_data_processed){
  reorder_factors = function(dat){
    dat %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")),
             type = factor(type, levels = c("control", "sorbed-C", "solution-C")))
  }
  
  ## make a subset with only the pieces needed
  combined_data2 = 
    combined_data_processed %>% 
    filter(treatment == "2-wetting") %>% 
    mutate(C13_ug_g = C13_mg_g * 1000) %>% 
    dplyr::select(core, type, treatment, fraction, C_mg_g, d13C_VPDB, C13_ug_g) %>% 
    reorder_factors(.)
  

  
  ## do stats for label
  make_hsd_labels = function(combined_data2){
    # a. make longform
    combined_data2_longform = 
      combined_data2 %>% 
      pivot_longer(-c(core, type, treatment, fraction), names_to = "variable", values_to = "value") %>% 
      force()
    
    # a. set y-axis values
    label_y = tribble(
      ~analysis, ~fraction, ~variable, ~y,
      "HSD",  "respiration", "d13C_VPDB", 2100,
      "HSD",  "weoc", "d13C_VPDB", 30,
      "HSD",  "weoc_pellet", "d13C_VPDB", -20,
      "HSD",  "soil", "d13C_VPDB", -20,
      
      "HSD",  "respiration", "C_mg_g", 0.154,
      "HSD",  "weoc", "C_mg_g", 0.15,
      "HSD",  "weoc_pellet", "C_mg_g", 30,
      "HSD",  "soil", "C_mg_g", 34,
      
      "HSD",  "respiration", "C13_ug_g", 3.5,
      "HSD",  "weoc", "C13_ug_g", 1.5,
      "HSD",  "weoc_pellet", "C13_ug_g", 320,
      "HSD",  "soil", "C13_ug_g", 360,
      
    )
    
    # b. then, calculate HSD for labels 
    fit_hsd = function(dat){
      a = aov(value ~ type, data = dat)
      h = HSD.test(a, "type")
      h$groups %>% mutate(type = row.names(.)) %>% 
        rename(label = groups) %>%  
        dplyr::select(type, label)
    }
    
    hsd_label = 
      combined_data2_longform %>% 
      group_by(treatment, fraction, variable) %>% 
      do(fit_hsd(.)) # %>% pivot_wider(names_from = "variable", values_from = "label")
    
    # c. then, merge the labels with the y
    hsd_label %>% left_join(label_y) %>% 
      mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil"))) %>% 
      reorder_factors(.)
      
  }
  hsd_labels = make_hsd_labels(combined_data2)
  
  ## make plots
  make_plots = function(combined_data2){
    
    plot_d13c = 
      combined_data2 %>% 
      ggplot(aes(x = type, y = d13C_VPDB))+
      geom_point(#aes(color = type), 
                 size = 3)+
      geom_text(data = hsd_labels %>% filter(variable == "d13C_VPDB"), aes(y = y, label = label))+
      facet_grid(fraction ~., scales = "free_y")+
      labs(x = "", y = "",
           title = "δ13C, \u2030")+
      theme_kp()+
      theme(legend.position = "none",
            strip.text.y = element_blank())+
      NULL
    
    plot_c = 
      combined_data2 %>% 
      ggplot(aes(x = type, y = C_mg_g))+
      geom_point(#aes(color = type), 
                 size = 3)+
      geom_text(data = hsd_labels %>% filter(variable == "C_mg_g"), aes(y = y, label = label))+
      facet_grid(fraction ~., scales = "free_y")+
      labs(x = "", y = "",
           title = "C, mg/g")+
      theme_kp()+
      theme(legend.position = "none")+
      theme(strip.text.y = element_blank())+
      NULL
      
    plot_c13 = 
      combined_data2 %>% 
      ggplot(aes(x = type, y = C13_ug_g))+
      geom_point(#aes(color = type), 
                 size = 3)+
      geom_text(data = hsd_labels %>% filter(variable == "C13_ug_g"), aes(y = y, label = label))+
      facet_grid(fraction ~., scales = "free_y")+
      labs(x = "", y = "",
           title = "C13, μg/g")+
      theme_kp()+
      theme(legend.position = "none")+
      NULL
      
    plot_c + plot_d13c + plot_c13
    
  }
  make_plots(combined_data2)
}
plot_sorbed_and_solution(combined_data_processed)


plot_mass_balance_sorbed_and_solution_OLD = function(combined_data_processed){
  ## first, subset only the necessary data
  combined_data2 = 
    combined_data_processed %>% 
    filter(treatment == "2-wetting") %>% 
    mutate(C13_ug_g = C13_mg_g * 1000) %>% 
    dplyr::select(core, type, treatment, fraction, C_mg_g, d13C_VPDB, C13_ug_g) %>% 
    reorder_factors(.)
  
  ## prepare summary
  combined_data2_summary = 
    combined_data2 %>%
    filter(fraction != "weoc") %>% 
    group_by(core) %>% 
    group_by(treatment, type, fraction) %>%
    dplyr::summarise(C_mg_g = mean(C_mg_g),
                     C13_ug_g = mean(C13_ug_g)) %>%
    mutate(across(where(is.numeric), round, 2))
  
  ## prepare labels
  mass_balance_label = 
    combined_data2_summary %>% 
    mutate(y_ug = case_when(fraction == "respiration" ~ 355,
                            fraction == "soil" ~ 200),
           label_ug = paste(fraction, "\n", C13_ug_g),
           y_mg = case_when(fraction == "respiration" ~ 33,
                            fraction == "soil" ~ 15),
           label_mg = paste(fraction, "\n", C_mg_g))
  
  mass_balance_label_total = 
    combined_data2_summary %>%
    group_by(treatment, type) %>% 
    dplyr::summarise(total_C = sum(C_mg_g),
                     total_C13 = sum(C13_ug_g)) %>% 
    mutate(label_C = paste("total:", total_C, "mg/g"),
           label_C13 = paste("total:", total_C13, "μg/g"))
  
  mass_balance_tukey_label =
    tribble(
      ~x, ~y, ~label,
      "control", 330, "(b)",
      "sorbed-C", 330, "(b)",
      "solution-C", 330, "(a)",
      
      "control", 180, "(A)",
      "sorbed-C", 180, "(B)",
      "solution-C", 180, "(B)",
    )
  ## make plots
  massbalance_c13 = 
    combined_data2_summary %>% 
    filter(fraction != "weoc") %>% 
    ggplot(aes(x = type, y = C13_ug_g))+
    geom_bar(aes(fill = fraction, color = fraction), 
             stat = "identity", #position = position_dodge(width = 0.7), 
             width = 0.5, alpha = 0.7, size = 0.7)+
    geom_text(data = mass_balance_label, aes(y = y_ug, label = label_ug))+
    geom_text(data = mass_balance_label_total, aes(y = 2, label = label_C13),
              nudge_x = -0.32, angle = 90, hjust = 0)+
    geom_text(data = mass_balance_tukey_label, aes(x = x, y = y, label = label))+
    labs(x = "", y = "13C (μg/g)",
         caption = "sorbed-C: 7.72 μg/g 13C added \n solution-C: 6.02 μg/g 13C added")+
    scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
    scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
    theme_kp()+
    NULL
  
  massbalance_c = 
    combined_data2_summary %>% 
    filter(fraction != "weoc") %>% 
    ggplot(aes(x = type, y = C_mg_g))+
    geom_bar(aes(fill = fraction, color = fraction), 
             stat = "identity", #position = position_dodge(width = 0.7), 
             width = 0.5, alpha = 0.7, size = 0.7)+
    geom_text(data = mass_balance_label, aes(y = y_mg, label = label_mg))+
    geom_text(data = mass_balance_label_total, aes(y = 2, label = label_C),
              nudge_x = -0.32, angle = 90, hjust = 0)+
    labs(x = "", y = "C (mg/g)",
         caption = "sorbed-C: 0.0125 mg/g 13C added \n solution-C: 0.04 mg/g C added")+
    scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
    scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
    theme_kp()+
    NULL
  
  list(massbalance_c13 = massbalance_c13,
       massbalance_c = massbalance_c)
}

plot_mass_balance_sorbed_and_solution = function(combined_data_processed){
  ## first, subset only the necessary data
  combined_data2 = 
    combined_data_processed %>% 
    filter(treatment == "2-wetting") %>% 
    mutate(C13_ug_g = C13_mg_g * 1000) %>% 
    dplyr::select(core, type, treatment, fraction, C_mg_g, d13C_VPDB, C13_ug_g) %>% 
    reorder_factors(.)
  
  ## prepare summary
  combined_data2_summary = 
    combined_data2 %>%
    filter(fraction != "soil") %>% 
    group_by(core) %>% 
    group_by(treatment, type, fraction) %>%
    dplyr::summarise(C_mg_g = mean(C_mg_g),
                     C13_ug_g = mean(C13_ug_g)) %>%
    mutate(across(where(is.numeric), round, 2))
  
  ## prepare summary table
  combined_data2_summarytable = 
    combined_data2 %>%
    filter(fraction != "soil") %>% 
    dplyr::select(-C_mg_g, -d13C_VPDB) %>% 
    group_by(core) %>% 
    dplyr::mutate(total = sum(C13_ug_g)) %>% 
    pivot_wider(names_from = "fraction", values_from = "C13_ug_g") %>% 
    pivot_longer(-c(core:treatment), names_to = "fraction", values_to = "C13_ug_g") %>% 
    group_by(treatment, type, fraction) %>%
    dplyr::summarise(C13_ug_g_mean = mean(C13_ug_g, na.rm = TRUE),
                     se = sd(C13_ug_g, na.rm = TRUE)/sqrt(n())) %>%
    mutate(across(where(is.numeric), round, 2))
  
  ## do HSD on total 
  total = combined_data2 %>%
    filter(fraction != "soil") %>% 
    dplyr::select(-C_mg_g, -d13C_VPDB) %>% 
    group_by(core, type, treatment) %>% 
    dplyr::summarize(total = sum(C13_ug_g)) 
  
  total_aov = aov(total ~ type, data = total)
  h = HSD.test(total_aov, "type")
  
  ## prepare labels
  mass_balance_label = 
    combined_data2_summary %>% 
    mutate(y_ug = case_when(fraction == "respiration" ~ 355,
                            fraction == "soil" ~ 200),
           label_ug = paste(fraction, "\n", C13_ug_g),
           y_mg = case_when(fraction == "respiration" ~ 33,
                            fraction == "soil" ~ 15),
           label_mg = paste(fraction, "\n", C_mg_g))
  
  mass_balance_label_total = 
    combined_data2_summary %>%
    group_by(treatment, type) %>% 
    dplyr::summarise(total_C = sum(C_mg_g),
                     total_C13 = sum(C13_ug_g)) %>% 
    mutate(label_C = paste("total:", total_C, "mg/g"),
           label_C13 = paste("total:", total_C13, "μg/g"))
  
  mass_balance_tukey_label =
    tribble(
      ~x, ~y, ~label,
      "control", 330, "(b)",
      "sorbed-C", 330, "(b)",
      "solution-C", 330, "(a)",
      
      "control", 180, "(A)",
      "sorbed-C", 180, "(B)",
      "solution-C", 180, "(B)",
    )
  ## make plots
  massbalance_c13 = 
    combined_data2_summary %>% 
    filter(fraction != "soil") %>% 
    ggplot(aes(x = type, y = C13_ug_g))+
    geom_bar(aes(fill = fraction, color = fraction), 
             stat = "identity", #position = position_dodge(width = 0.7), 
             width = 0.5, alpha = 0.7, size = 0.7)+
    # geom_text(data = mass_balance_label, aes(y = y_ug, label = label_ug))+
    geom_text(data = mass_balance_label_total, aes(y = 2, label = label_C13),
              nudge_x = -0.32, angle = 90, hjust = 0)+
    # geom_text(data = mass_balance_tukey_label, aes(x = x, y = y, label = label))+
    labs(x = "", y = "13C (μg/g)",
         caption = "sorbed-C: 7.72 μg/g 13C added \n solution-C: 6.02 μg/g 13C added")+
    scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
    scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
    facet_zoom(ylim = c(260, 300))+
    #theme_kp()+
    NULL
  
  massbalance_c = 
    combined_data2_summary %>% 
    filter(fraction != "soil") %>% 
    ggplot(aes(x = type, y = C_mg_g))+
    geom_bar(aes(fill = fraction, color = fraction), 
             stat = "identity", #position = position_dodge(width = 0.7), 
             width = 0.5, alpha = 0.7, size = 0.7)+
    # geom_text(data = mass_balance_label, aes(y = y_mg, label = label_mg))+
    geom_text(data = mass_balance_label_total, aes(y = 2, label = label_C),
              nudge_x = -0.32, angle = 90, hjust = 0)+
    labs(x = "", y = "C (mg/g)",
         caption = "sorbed-C: 0.0125 mg/g 13C added \n solution-C: 0.04 mg/g C added")+
    scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
    scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
    facet_zoom(ylim = c(25, 28))+
    #theme_kp()+
    NULL
  
  list(massbalance_c13 = massbalance_c13,
       massbalance_c = massbalance_c)
}
plot_mass_balance_sorbed_and_solution(combined_data_processed)


###############
###############

# test for soil C heterogeneity -------------------------------------------

het_soil_irms = read.csv("data/irms/irms_soil_heterogeneity_2021-02-25.csv", na.strings = "")
het_soil_tc = read.csv("data/irms/tc_soil_heterogeneity_2021-02-25.csv", na.strings = "")
core_key = read.csv(("data/core_key.csv")) %>% mutate(core = as.character(core)) %>% filter(is.na(skip)) %>% 
  dplyr::select(core, type, treatment)

process_heterogeneity_data = function(het_soil_tc, het_soil_irms){
  
  # initial cleaning
  tc = 
    het_soil_tc %>% 
    filter(sample_type == "sample") %>% 
    dplyr::select(Name, C_perc) %>% 
    rename(name = Name)
  
  irms = 
    het_soil_irms %>% 
    filter(sample_group == "sample") %>% 
    dplyr::select(name, d13C_VPDB)
  
  # combine
  R13C_VPDB = 0.011237
  
  combined = 
    left_join(tc, irms, by = "name") %>% 
    filter(!grepl("-rep", name)) %>% 
    mutate(core = as.character(parse_number(name))) %>% 
    dplyr::select(name, core, C_perc, d13C_VPDB) %>% 
    left_join(core_key, by = "core") %>% 
    mutate(
      C_mg_g = C_perc * 10,
      R13C = ((d13C_VPDB/1000) + 1) * R13C_VPDB,
      F13C = R13C/(1+R13C),
      R13C = round(R13C, 4),
      F13C = round(F13C, 4),
      C13_mg_g = F13C * C_mg_g,
      C13_ug_g = C13_mg_g*1000)
  
  summary_tc = 
    combined %>% 
    group_by(core, type, treatment) %>% 
    dplyr::summarise(mean_tc_mg_g = mean(C_mg_g),
                     sd = sd(C_mg_g),
                     se = sd/sqrt(n()),
                     cv = (sd/mean_tc_mg_g)*100) %>% 
    mutate_if(is.numeric, ~round(.,2))
  
  summary_c13 = 
    combined %>% 
    group_by(core, type, treatment) %>% 
    dplyr::summarise(mean_c13_ug_g = mean(C13_ug_g),
                     sd = sd(C13_ug_g),
                     se = sd/sqrt(n()),
                     cv = (sd/mean_c13_ug_g)*100) %>% 
    mutate_if(is.numeric, ~round(.,2))
  
  
  summary_d13c = 
    combined %>% 
    group_by(core, type, treatment) %>% 
    dplyr::summarise(mean_d13C = mean(d13C_VPDB),
                     sd = sd(d13C_VPDB*-1),
                     se = sd/sqrt(n()),
                     cv = (sd/mean_d13C)*100) %>% 
    mutate_if(is.numeric, ~round(.,2))
    
  
  combined_longform = 
    combined %>% 
    dplyr::select(core, type, treatment, C_mg_g, d13C_VPDB, C13_ug_g) %>% 
    pivot_longer(-c(core, type, treatment), values_to = "value", names_to = "name")  %>% 
    mutate(type = factor(type, levels = c("control", "sorbed-C", "solution-C")))
  
  combined_longform %>% 
    ggplot(aes(x = type, y = value, group = core, color = core, shape = type))+
    geom_point(size = 2, stroke = 1, position = position_dodge(width = 0.6), show.legend = F)+
    scale_shape_manual(values = c(1, 16, 4))+
    facet_wrap(~name, scales = "free_y")+
    labs(x = "", y = "")+
    theme_kp()+
    NULL
  
}





calculate_mdc = function(tail, alpha, power, pre, post){
  n_pre = length(pre)
  n_post = length(post)
  
  b = 1-power
  
  MSE_pre = sd(pre)^2
  MSE_post = sd(post)^2
  
  ta = qt(1 - (2*b/2), n_pre+n_post-2)
  tb = qt(1 - (alpha/tail), n_pre+n_post-2)
  
  
  (ta + tb) * (sqrt((MSE_pre/n_pre) + (MSE_post/n_post)))
  
}
calculate_mdc(tail = 2,
              alpha = 0.05,
              power = 0.80,
              pre = combined %>% filter(core %in% c(21, 23, 25)) %>% pull(C13_ug_g),
              post = combined %>% filter(core %in% c(6, 7, 9)) %>% pull(C13_ug_g)
              )


calculate_mdc(tail = 2,
              alpha = 0.05,
              power = 0.80,
              pre = combined %>% filter(core %in% c(21, 23, 25)) %>% pull(C_mg_g),
              post = combined %>% filter(core %in% c(6, 7, 9)) %>% pull(C_mg_g)
)
