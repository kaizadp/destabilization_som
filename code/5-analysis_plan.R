#core_weights = read.csv("data/processed/core_weights.csv") %>% mutate(core = as.character(core))


source("code/4a-analysis_functions.R")


analysis_plan = drake_plan(
  # 1 load files
  soil = read.csv(file_in(SOIL_PROCESSED)),
  weoc = read.csv(file_in(WEOC_PROCESSED)),
  weoc_pellet = read.csv(file_in(WEOC_PELLETS_PROCESSED)),
  respiration = read.csv(file_in(RESPIRATION_PROCESSED)),
  core_key = read.csv(file_in("data/core_key.csv")) %>% mutate(core = as.character(core)) %>% filter(is.na(skip)),
  core_weights = read.csv("data/processed/core_weights.csv"),
  
  
  # 2 combine and clean
  combined_data = combine_data(soil, weoc, weoc_pellet, respiration, core_key, core_weights),
  combined_data_outliers = remove_outliers(combined_data),
  combined_data_processed = calculate_indices(combined_data_outliers),
  
  # 3 
  #gg_prelim = make_graphs_prelim(combined_data_processed),
  # gg_desorption = make_graphs_desorption(combined_data_processed),
  gg_priming = plot_sorbed_and_solution(combined_data_processed),
  
  #gg_d13c = make_graphs_d13c(combined_data_processed),
  #gg_c = make_graphs_c(combined_data_processed),
  
  # 4 mass balance
  gg_mass_balance = plot_mass_balance_sorbed_and_solution(combined_data_processed),
  
  # 5 clay effect
  #  gg_tzero = calculate_clay_effect(combined_data_processed),
  
  # report 
  report = rmarkdown::render(
    knitr_in("reports/report_priming.Rmd"), output_format = rmarkdown::github_document()),
)

make(analysis_plan)
