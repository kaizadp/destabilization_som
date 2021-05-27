source("code/0-packages.R")
library(outliers)
library(agricolae)


reorder_factors = function(dat){
  dat %>% 
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "soil")),
           type = factor(type, levels = c("control", "sorbed-C", "solution-C")))
}

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
    dplyr::select(core, fraction, C_mg_g, d13C_VPDB) %>% 
    mutate(core = as.character(core))
  
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
  hsd_labels = make_hsd_labels(combined_data2) %>% 
    filter(fraction %in% c("respiration", "weoc"))
    
  
  ## make plots
  make_plots = function(combined_data2){
    
    plot_c = 
      combined_data2 %>%       
      filter(fraction %in% c("respiration", "weoc")) %>% 
      ggplot(aes(x = type, y = C_mg_g))+
      geom_point(#aes(color = type), 
        size = 3)+
      geom_text(data = hsd_labels %>% filter(variable == "C_mg_g"), aes(y = y, label = label))+
      facet_grid(fraction ~., scales = "free_y")+
      labs(x = "", y = "",
           title = expression(bold("C, mg g" ^-1)))+
      scale_x_discrete(# limits = c("1-time-zero", "2-wetting"),
        breaks = c("control", "sorbed-C", "solution-C"),
        labels = c("control", "+MAOC", "+DOC"))+
      theme_kp()+
      theme(legend.position = "none")+
      theme(strip.text.y = element_blank())+
      NULL
    
    plot_d13c = 
      combined_data2 %>% 
      filter(fraction %in% c("respiration", "weoc")) %>% 
      ggplot(aes(x = type, y = d13C_VPDB))+
      geom_point(#aes(color = type), 
        size = 3)+
      geom_text(data = hsd_labels %>% filter(variable == "d13C_VPDB"), aes(y = y, label = label))+
      facet_grid(fraction ~., scales = "free_y",
                 labeller = as_labeller(c('respiration' = "respiration", 'weoc' = "WEOC")))+
      labs(x = "", y = "",
           title = expression(bold(delta ^13 * "C, \u2030")))+
      scale_x_discrete(# limits = c("1-time-zero", "2-wetting"),
        breaks = c("control", "sorbed-C", "solution-C"),
        labels = c("control", "+MAOC", "+DOC"))+
      theme_kp()+
      theme(legend.position = "none",
            #strip.text.y = element_blank()
      )+
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
    
    plot_c + plot_d13c # + plot_c13
    
  }
  make_plots(combined_data2)
}
# plot_sorbed_and_solution(combined_data_processed)

plot_mass_balance_sorbed_and_solution = function(combined_data_processed){
  ## first, subset only the necessary data
  combined_data2 = 
    combined_data_processed %>% 
    filter(treatment == "2-wetting") %>% 
    mutate(C13_ug_g = C13_mg_g * 1000) %>% 
    dplyr::select(core, type, treatment, fraction, C_mg_g, d13C_VPDB, C13_ug_g) %>% 
    reorder_factors(.)
  
  ## prepare summary ----
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
    mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "weoc_pellet", "total"))) %>% 
    group_by(treatment, type, fraction) %>%
    dplyr::summarise(C13_ug_g_mean = mean(C13_ug_g, na.rm = TRUE),
                     se = sd(C13_ug_g, na.rm = TRUE)/sqrt(n())) %>%
    mutate(across(where(is.numeric), round, 2)) 
  
  ## stats and plot ----
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
  
  list(combined_data2_summarytable = combined_data2_summarytable,
       massbalance_c13 = massbalance_c13,
       massbalance_c = massbalance_c)
}
# plot_mass_balance_sorbed_and_solution(combined_data_processed)


###############
###############

# test for soil C heterogeneity -------------------------------------------

het_soil_irms = read.csv("data/irms/additional_tests/irms_soil_heterogeneity_2021-02-25.csv", na.strings = "")
het_soil_tc = read.csv("data/irms/additional_tests/tc_soil_heterogeneity_2021-02-25.csv", na.strings = "")
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
process_heterogeneity_data(het_soil_tc, het_soil_irms)




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
              pre = combined_data_processed %>% filter(type == "control" & fraction == "weoc_pellet") %>% 
                mutate(C13_ug_g = C13_mg_g*1000) %>% pull(C13_ug_g),
              post = combined_data_processed %>% filter(type == "sorbed-C" & fraction == "weoc_pellet") %>% 
                mutate(C13_ug_g = C13_mg_g*1000) %>% pull(C13_ug_g)
)


calculate_mdc(tail = 2,
              alpha = 0.05,
              power = 0.80,
              pre = combined_data_processed %>% filter(type == "control" & fraction == "weoc_pellet") %>% 
                mutate(C13_ug_g = C13_mg_g*1000) %>% pull(C13_ug_g),
              post = combined_data_processed %>% filter(type == "solution-C" & fraction == "weoc_pellet") %>% 
                mutate(C13_ug_g = C13_mg_g*1000) %>% pull(C13_ug_g)
)



# STATS -------------------------------------------------------------------

combined_data_processed %>% 
  filter(fraction == "respiration") %$% 
  aov((C_mg_g) ~ type) %>% 
  summary()

combined_data_processed %>% 
  filter(fraction == "respiration") %$% 
  aov((d13C_VPDB) ~ type) %>% 
  summary()

combined_data_processed %>% 
  group_by(type, fraction) %>% 
  dplyr::summarise(d13C = mean(d13C_VPDB))

combined_data_processed %>% 
  filter(fraction == "weoc" & type != "control") %$% 
  var.test(d13C_VPDB ~ type, alternative = "two.sided") 
