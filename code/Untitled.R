

# -------------------------------------------------------------------------


resp = 
  respiration %>% 
  mutate(R13C = ((D13C_VPDB_CO2/1000) + 1) * R13C_VPDB,
         F13C = R13C/(1+R13C),
         R13C = round(R13C, 4),
         F13C = round(F13C, 4),
         C13_umol = F13C * umol_CO2C,
         C12_umol = umol_CO2C - C13_umol,
         C13_ug = C13_umol*13,
         C12_ug = C12_umol*12,
         C_ug = C13_ug+C12_ug,
         C_ug_g = C_ug/60)


## 
## partial pressure ppm = umol CO2/mol air
## mol of air = 0.0177 

resp_licor_temp2 = 
  resp_licor_temp %>% 
  mutate(umol_CO2 = pCO2_ppm *0.0177)


loadd(combined_data_processed)

combined_data_processed %>% 
  filter(type == "control") %>% 
  ggplot(aes(x = treatment, y = d13C_VPDB))+
  geom_point()+
  facet_grid(fraction~., scales = "free_y")


# -------------------------------------------------------------------------

combined_data_processed %>% 
  mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
  filter(!type %in% c("control")) %>% 
  ggplot(aes(x = treatment, y = d13C_VPDB, color = type))+
  #geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
  geom_point(size = 3, show.legend = FALSE) +
  #scale_color_manual(values = pnw_palette("Sailboat", 3))+
  scale_color_manual(values = c(NA, "black"))+
  labs(title = "δ13C enrichment in each fraction")+
  scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
  facet_grid(fraction~., scales = "free_y")+
  theme_kp()+
  NULL


combined_data_processed %>% 
  mutate(fraction = factor(fraction, levels = c("respiration", "weoc", "soil"))) %>% 
  filter(!type %in% c("control")) %>% 
  filter(treatment == "3-drying") %>% 
  ggplot(aes(x = treatment, y = d13C_VPDB, color = type))+
  geom_hline(data = control_summary, aes(yintercept = d13C_VPDB), linetype = "dashed", color = "grey30")+
  geom_point(size = 3, show.legend = FALSE) +
  #scale_color_manual(values = c(NA, "#e89c81"))+
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

test = combined_data_processed %>% 
  mutate(type2 = "TRT") %>% 
  rbind(control) %>% 
  filter(fraction == "respiration" & type == "sorbed-C" & treatment == "3-drying")

test %>% 
  ggplot(aes(x = type2, y = d13C_VPDB))+
  geom_point()

aov(d13C_VPDB ~ type2, data = test)  %>% broom::tidy()

test %>% 
  group_by(type2) %>%  dplyr::summarise(d13C = mean(d13C_VPDB))


# -------------------------------------------------------------------------

## calculate MDC ----
# https://www.epa.gov/sites/production/files/2015-10/documents/tech_memo_3_oct15.pdf
# http://www.ttable.org/student-t-value-calculator.html

combined_data_processed %>% 
  mutate(C13_ug_g = C13_mg_g*1000) %>% 
  dplyr::select(-C_mg_g, -C13_mg_g) %>% 
  filter(type == "control" & fraction == "soil" & treatment == "1-time-zero") %>% 
  summarise(sd = sd(C13_ug_g))


combined_data_processed %>% 
  mutate(C13_ug_g = C13_mg_g*1000) %>% 
  dplyr::select(-C_mg_g, -C13_mg_g) %>% 
  filter(type == "sorbed-C" & fraction == "soil" & treatment == "1-time-zero") %>% 
  summarise(sd = sd(C13_ug_g))


(1.8595+0.8889) * sqrt(((6.54*6.54) + (17.8*17.8))/5)  #1t
(0.0647+0.2619) * sqrt(((6.54*6.54) + (17.8*17.8))/5)  #2t

## minimum detectable change in soil 13C is 2.76 ug/g (by the 2-tailed test)


## calculate MDC part 2 ----
## here, we assume that total 13C in soil will not change by treatment. so we calculate a total SD across all four treatments



tail = 2



a = 0.05
power = 0.80

pre = combined_data_processed %>% filter(type == "control" & fraction == "soil") %>% mutate(C13_ug_g = C13_mg_g*1000)%>% pull(C13_ug_g)
post = combined_data_processed %>% filter(type == "sorbed-C" & fraction == "soil") %>% mutate(C13_ug_g = C13_mg_g*1000)%>% pull(C13_ug_g)



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

loadd(combined_data_processed)
calculate_mdc(tail = 2,
              alpha = 0.05,
              power = 0.80,
              pre = combined_data_processed %>% filter(type == "control" & fraction == "soil"& treatment == "2-wetting") %>% mutate(C13_ug_g = C13_mg_g*1000)%>% pull(C13_ug_g), 
              post = combined_data_processed %>% filter(type == "solution-C" & fraction == "soil"& treatment == "2-wetting") %>% mutate(C13_ug_g = C13_mg_g*1000)%>% pull(C13_ug_g))

## comparing solution-C 1-time-zero vs. 2-wetting, MDC = 22.89 for a = 0.05 and power = 0.80



#
# checking solution C T0 stats --------------------------------------------

loadd(combined_data_processed)

solution_tzero = 
  combined_data_processed %>% 
  filter(treatment == "1-time-zero" & type != "sorbed-C")

solution_tzero %>% 
  ggplot(aes(x = type, y = d13C_VPDB))+
  geom_point()+
  facet_grid(fraction~., scales = "free")+
  NULL

aov(d13C_VPDB ~ type, data = solution_tzero %>% filter(fraction == "respiration")) %>% summary()
aov(d13C_VPDB ~ type, data = solution_tzero %>% filter(fraction == "soil")) %>% summary()


combined_data_processed %>% 
  filter(treatment == "1-time-zero" & type == "control") %>% 
  group_by(fraction) %>% 
  dplyr::summarise(n = n(),
                   C = mean(C_mg_g)*10,
                   d13C = mean(d13C_VPDB),
                   R = mean(R13C))


## convert R to at% ----

R = C13/C12
at = (C13(C12+C13)) * 100
at = (R13C/(1+R13C)) * 100

R13C = 0.0109


# -------------------------------------------------------------------------


combined_data_processed_summary %>% 
  filter(type != "solution-C" & !(fraction == "respiration" & treatment == "3-drying")) %>% 
  ggplot(aes(x = treatment, y = C13_mg_g*1000))+
  geom_bar(aes(fill = fraction, color = fraction), stat = "identity", position = position_dodge(width = 0.7), 
           width = 0.5, alpha = 0.7, size = 0.7)+
  #geom_text(data = label %>%  filter(type != "solution-C"), aes(y = 2.95, label = C13_mg_g*1000))+
  #annotate("text", label = "total 13C in soil (μg/g):", x = 0.7, y = 3.10, hjust = 0)+
  labs(x = "", y = "13C (μg/g)")+
  scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
  scale_y_log10()+
  #scale_fill_manual(values = pnw_palette("Sunset", 3))+
  scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
  scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
  facet_wrap(~type)+
  theme_kp()+
  #theme(axis.text.x = element_text(angle = 45))+
  NULL



combined_data_processed %>% 
  filter(type != "solution-C" & !(fraction == "respiration" & treatment == "3-drying")) %>% 
  ggplot(aes(x = treatment, y = C13_mg_g*1000))+
  #    geom_bar(aes(fill = fraction, color = fraction), stat = "identity", position = position_dodge(width = 0.7), 
  #             width = 0.5, alpha = 0.7, size = 0.7)+
  geom_point(aes(color = fraction))+
  #geom_text(data = label %>%  filter(type != "solution-C"), aes(y = 2.95, label = C13_mg_g*1000))+
  #annotate("text", label = "total 13C in soil (μg/g):", x = 0.7, y = 3.10, hjust = 0)+
  labs(x = "", y = "13C (μg/g)")+
  scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
  scale_y_log10()+
  #scale_fill_manual(values = pnw_palette("Sunset", 3))+
  scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
  scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
  facet_wrap(~type)+
  theme_kp()+
  #theme(axis.text.x = element_text(angle = 45))+
  NULL


combined_data_processed %>% 
  filter(type != "solution-C" & !(treatment == "3-drying")) %>%
  group_by(type, fraction, treatment) %>% 
  dplyr::summarise(C13 = mean(C13_mg_g*1000)) %>% 
  pivot_wider(names_from = "type", values_from = "C13") %>% 
  mutate(diff_ug_g = `sorbed-C` - control) %>% 
  
  ggplot(aes(x = treatment, y = diff_ug_g))+
  geom_point(aes(color = fraction))+
  labs(x = "", y = "13C difference (μg/g)")+
  scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
  #scale_y_log10()+
  #scale_fill_manual(values = pnw_palette("Sunset", 3))+
  scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
  scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
  #facet_wrap(~type)+
  theme_kp()+
  #theme(axis.text.x = element_text(angle = 45))+
  NULL



combined_data_processed %>% 
  filter(type != "sorbed-C" & (treatment %in% c("1-time-zero", "2-wetting"))) %>%
  group_by(type, fraction, treatment) %>% 
  dplyr::summarise(C13 = mean(C13_mg_g*1000)) %>% 
  pivot_wider(names_from = "type", values_from = "C13") %>% 
  mutate(diff_ug_g = `solution-C` - control) %>% 
  
  ggplot(aes(x = treatment, y = diff_ug_g))+
  geom_point(aes(color = fraction))+
  labs(x = "", y = "13C difference (μg/g)")+
  scale_x_discrete(labels = c("T0", "+C"))+
  #scale_y_log10()+
  #scale_fill_manual(values = pnw_palette("Sunset", 3))+
  scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
  scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
  #facet_wrap(~type)+
  theme_kp()+
  #theme(axis.text.x = element_text(angle = 45))+
  NULL



combined_data_processed %>% 
  #filter(type != "sorbed-C" & (treatment %in% c("1-time-zero", "2-wetting"))) %>%
  group_by(type, fraction, treatment) %>% 
  dplyr::summarise(C13 = mean(C13_mg_g*1000)) %>% 
  pivot_wider(names_from = "type", values_from = "C13") %>% 
  mutate(diff_sol_ug_g = `solution-C` - control,
         diff_sor_ug_g = `sorbed-C` - control) %>% 
  dplyr::select(fraction, treatment, starts_with("diff")) %>% 
  pivot_longer(-c(fraction, treatment), values_to = "diff_ug_g", names_to = "type") %>% 
  
  ggplot(aes(x = treatment, y = diff_ug_g))+
  geom_point(aes(color = fraction))+
  labs(x = "", y = "13C difference (μg/g)")+
  scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
  #scale_y_log10()+
  #scale_fill_manual(values = pnw_palette("Sunset", 3))+
  scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
  scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
  facet_wrap(~type)+
  theme_kp()+
  #theme(axis.text.x = element_text(angle = 45))+
  NULL



combined_data_processed %>% 
  filter(!(type == "sorbed-C" & treatment == "3-drying" & fraction == "respiration")) %>%
  group_by(type, fraction, treatment) %>% 
  
  ggplot(aes(x = treatment, y = C13_mg_g*1000))+
  geom_point(aes(color = fraction), position = position_dodge(width = 0.3))+
  labs(x = "", y = "13C difference (μg/g)")+
  scale_x_discrete(labels = c("T0", "W", "D", "DW"))+
  scale_y_log10()+
  #scale_fill_manual(values = pnw_palette("Sunset", 3))+
  scale_fill_manual(values = soilpalettes::soil_palette("redox2", 3))+
  scale_color_manual(values = soilpalettes::soil_palette("redox2", 3))+
  facet_wrap(~type)+
  theme_kp()+
  #theme(axis.text.x = element_text(angle = 45))+
  NULL



solutionc = combined_data_processed %>% filter(type == "solution-C")


summary(aov((C13_mg_g) ~ treatment, data = solutionc %>%  filter(fraction == "weoc")))  



goethite = tribble(
  ~g_perc, ~d13C,
  1.5, -27.63,
  1.5, -27.72,
  1.5, -29.30,
  7.5, -30.46,
  7.5, -27.41,
  7.5, -26.88,
  13, -26.03,
  13, -24.42,
  13, -28.01,
  20, -26.62,
  50, -27.43,
  0, -27.00,
  0, -27.32,
  100, -23.68,
  100, -23.80,
  100, -24.20
)


goethite %>% 
  ggplot(aes(x = g_perc, y = d13C))+
  geom_point()



# alternate drying/rewetting plot -----------------------------------------

drying_rewetting_data = 
  combined_data_processed %>% 
  mutate(trt = case_when(type == "control" & treatment == "1-time-zero" ~ "baseline",
                         type == "sorbed-C" & (treatment == "1-time-zero" | treatment == "2-wetting") ~ "wetting",
                         type == "sorbed-C" & (treatment == "3-drying" | treatment == "4-drying-rewetting") ~ "drying")) %>% 
  filter(!is.na(trt)) %>% 
  mutate(level = case_when(trt == "baseline" ~ "baseline",
                           treatment == "1-time-zero" | treatment == "3-drying" ~ "TO",
                           treatment == "2-wetting" | treatment == "4-drying-rewetting" ~ "wet",
                           ))

drying_rewetting_data %>% 
  ggplot(aes(x = trt, y = d13C_VPDB, color = level))+
  geom_point(position = position_dodge(width = 0.4))+
  facet_grid(fraction ~., scales = "free_y")+
  theme_kp()

drying_rewetting_data %>% 
  ggplot(aes(x = trt, y = C_mg_g, color = level))+
  geom_point(position = position_dodge(width = 0.4))+
  facet_grid(fraction ~., scales = "free_y")+
  theme_kp()

