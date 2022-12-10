library(tidyverse)
library(srvyr)

# clean data with weights
data_path <- "inputs/clean_data_unhcr_pa.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_ref_with_weights <- readxl::read_excel(path = data_path, sheet = "UGA2207_PA", col_types = c_types)
df_roster_with_weights <- readxl::read_excel(path = data_path, sheet = "hh_roster")

# tool
df_survey <- readxl::read_excel("inputs/participatory_assessment_tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey |> 
  select(type, name, label) |> 
  filter(str_detect(string = type, pattern = "integer|select_one|select_multiple"))

# dap
dap <- read_csv("inputs/r_dap_uga_pa.csv")

# set up design objects
ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights )

df_main_analysis <- analysis_support_after_survey_creation(input_ref_svy = ref_svy,
                                                           input_dap = dap |> filter(!variable %in% c("gender", "age", "i.age")))

df_roster_analysis <- analysis_support_after_survey_creation(input_ref_svy = df_roster_with_weights,
                                                           input_dap = dap |> filter(variable %in% c("gender", "age", "i.age")))

# merge analysis ----------------------------------------------------------

combined_analysis <- bind_rows(df_main_analysis, df_roster_analysis)
 
full_analysis_long <- combined_analysis |> 
  

full_analysis_long |>
  write_csv(paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_caregiver.csv"), na="")