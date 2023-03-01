library(tidyverse)
library(srvyr)

source("R/support_functions.R")

# clean data with weights
data_path <- "inputs/clean_data_unhcr_pa.xlsx"

data_nms <- names(readxl::read_excel(path = data_path, n_max = 2000, sheet = "UGA2207_PA"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "UGA2207_PA", col_types = c_types, na = "NA")
df_roster_clean_data <- readxl::read_excel(path = data_path, sheet = "hh_roster", na = "NA")

# tool
df_survey <- readxl::read_excel("inputs/participatory_assessment_tool.xlsx", sheet = "survey") 

df_tool_data_support <- df_survey |> 
  select(type, name, label) |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_uga_pa.csv") |> 
  filter(!str_detect(string = subset_1, pattern = " |household_type"))

df_question_type_data <- read_csv("inputs/r_question_groups_pa.csv")

# set up design objects
ref_svy <- as_survey(.data = df_main_clean_data)

df_main_analysis <- analysis_support_after_survey_creation(input_ref_svy = ref_svy,
                                                           input_dap = dap |> filter(!variable %in% c("gender", "age", "i.age")))

# df_roster_analysis <- analysis_support_after_survey_creation(input_ref_svy = df_roster_clean_data,
#                                                            input_dap = dap |> filter(variable %in% c("gender", "age", "i.age")))

# merge analysis ----------------------------------------------------------

# combined_analysis <- bind_rows(df_main_analysis, df_roster_analysis)
combined_analysis <- df_main_analysis
 
full_analysis_long <- combined_analysis |> 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) %>% 
  left_join(df_tool_data_support, by = c("int.variable" = "name")) %>% 
  relocate(label, .after = variable) %>% 
  mutate(label = ifelse(is.na(label), variable, label),
         `mean/pct` = ifelse(select_type %in% c("integer") & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) %>%
  left_join(df_question_type_data, by = c("int.variable" = "kobo_question")) |> 
  mutate(indicator_group_sector = ifelse(variable == "i.disability", "Demographics", indicator_group_sector)) |> 
  select(`Question`= label, `Question Group/Sector` = indicator_group_sector, variable, `choices/options` = variable_val, `Results(mean/percentage)` = `mean/pct`, n_unweighted, population, subset_1_name, subset_1_val)

write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_pa.csv"), na="")
write_csv(full_analysis_long, paste0("outputs/full_analysis_lf_pa.csv"), na="")
