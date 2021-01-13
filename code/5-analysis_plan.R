core_weights = read.csv("data/processed/core_weights.csv") %>% mutate(core = as.character(core))


source("code/4a-analysis_functions.R")


analysis_plan = drake_plan(
  # 1 load files
  soil = read.csv(file_in(SOIL_PROCESSED)),
  weoc = read.csv(file_in(WEOC_PROCESSED)),
  respiration = read.csv(file_in(RESPIRATION_PROCESSED)),
  core_key = read.csv(file_in("data/core_key.csv")) %>% mutate(core = as.character(core)) %>% filter(skip != "skip"),
  core_weights = read.csv("data/processed/core_weights.csv"),

  
  # 2 combine and clean
  combined_data = combine_data(soil, weoc, respiration, core_key, core_weights),
  combined_data_outliers = remove_outliers(combined_data),
  combined_data_processed = calculate_indices(combined_data_outliers),
  
  # 3 
  gg_prelim = make_graphs_prelim(combined_data_processed),
  gg_desorption = make_graphs_desorption(combined_data_processed),
  gg_priming = make_graphs_priming(combined_data_processed),
  
  # report 
  report = rmarkdown::render(
    knitr_in("reports/slides.Rmd") #, output_format = rmarkdown::pdf_document()
    ),
  pagedown::chrome_print("reports/slides.html", "ninja3.pdf")
  
)

make(analysis_plan)
